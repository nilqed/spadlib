(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'COMPLXP)) 
(EXPORTS
 (LIST 'A2GF 'A2RAT 'ACCUPR 'BDSTEST 'BFPRIM 'BFRNDEM 'CALCPREC 'CSEP 'CVT2
       'CVT5 'DEFLATE1 'DEFLATE1C 'DSPLY 'GETROOT 'GFFINITR 'GFGETMIN 'GFSHIFT
       'GFSTORVAL 'INVPOLY 'LEADATOM 'LIMCHK 'MKGIRAT 'MKINTEG 'MKPOLY
       'ORGSHIFT 'P1RMULT 'RLRTNO2 'RRPWR 'RXGFC 'RXGFRL 'RXRL 'SCHNOK 'STURM1
       'UNGFFC 'XOSHIFT)) 
(IMPORTS
 (LIST '!MFEFIX '*F2Q '|ABS:| 'ASHIFT 'AUTOMOD 'BFABS 'BFDIVIDE 'BFIXUP
       'BFLESSP 'BFLOAT 'BFLOATEM 'BFNUMP 'BFNZP 'BFSQRT 'BFZP 'CEILING
       'CEILLOG 'CFLOT 'CFLOTEM 'CPXP 'DECIMAL2INTERNAL 'DEFLATE2 'DENR 'DIVBF
       '|DIVIDE:| 'DOMAINP '|EP:| 'EQCAR '|EQUAL:| 'ERRORP 'ERRORSET* 'FLOOR
       'FTOUT 'GCDN 'GEQ 'GF2BF 'GFFINIT 'GFIM 'GFPLUS 'GFPLUSN 'GFQUOTIENT
       'GFRSQ 'GFRTRND 'GFSIMP 'GFTIMES 'GFTIMESN 'GFVAL 'GFZEROP '|GREATERP:|
       'GTAG 'HYPOT 'INTDIFF 'LASTPAIR 'LCM '|LESSP:| 'LOG '|MAKE:IBF| 'MINBND1
       '|MINUS:| 'MK*SQ 'MKQUOTE 'MKRATL 'MKXCL '|MT:| 'N2GF 'NCOEFFS 'NCPXP
       'NEQ 'NUM 'NUMR 'PLUBF '|PLUS:| 'PMSG '|PRECI:| 'PREPSQ 'PRIMP 'PRIMPN
       'R2BF 'R2FLBF 'RATLESSP 'REALRAT 'RERROR 'REVAL 'REVERSIP 'RL2GF 'RNDPWR
       'ROOTRND '|ROUND:MT| 'SCH 'SCHINF 'SGN 'SGN1 'SIMP* 'SQRT 'STURM 'TIMBF
       '|TIMES:| 'TYPERR 'XCLP)) 
(FLUID '(|ACC#| *INTP *MULTRT *STRM |SPREC#| *XO *RVAR |ROOTACC##|)) 
(FLUID '(*BFTAG *ROOTMSG |LIMS#| |PFACTOR#| *XNLIST |ACCM#| |PFLT#|)) 
(GLOBAL '(|LIMLST#| BFONE*)) 
(PUT 'RXGFRL 'NUMBER-OF-ARGS 2) 
(PUT 'RXGFRL 'DEFINED-ON-LINE '63) 
(PUT 'RXGFRL 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'RXGFRL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RXGFRL (P X)
    ((LAMBDA (C)
       (PROGN
        (SETQ X (GFABS X))
        (PROG (I)
          (SETQ I (CDR P))
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (PROGN
              (SETQ C (GFTIMES C X))
              (COND
               (I
                (SETQ C
                        (GFPLUS C
                                ((LAMBDA (U)
                                   (COND
                                    (*BFTAG
                                     (CONS
                                      (COND ((FLOATP U) (FL2BF U))
                                            (T
                                             (NORMBF
                                              (COND ((NOT (ATOM U)) U)
                                                    ((FIXP U)
                                                     (CONS '|:RD:| (CONS U 0)))
                                                    (T (|READ:NUM| U))))))
                                      BFZ*))
                                    (T (CONS (CFLOT U) 0.0))))
                                 (COND ((ATOM I) (ABS I))
                                       (T (|ABS:| I))))))))))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB))
        (BFSQRT (GFRSQ C))))
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
      ((LAMBDA (U) (COND ((ATOM U) (ABS U)) (T (|ABS:| U))))
       (CAR (SETQ P (CDR (NCOEFFS P)))))))) 
(PUT 'RXGFC 'NUMBER-OF-ARGS 2) 
(PUT 'RXGFC 'DEFINED-ON-LINE '71) 
(PUT 'RXGFC 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'RXGFC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RXGFC (P X)
    ((LAMBDA (C)
       (PROGN
        (SETQ X (GFABS X))
        (PROG (I)
          (SETQ I (CDR P))
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (PROGN
              (SETQ C (GFABSTIM X C))
              (COND (I (SETQ C (GFPLUS C (GFABS I)))))))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB))
        (BFSQRT (GFRSQ C))))
     (GFABS (CAR (SETQ P (CDR (NCOEFFS P))))))) 
(PUT 'GFABS 'NUMBER-OF-ARGS 1) 
(PUT 'GFABS 'DEFINED-ON-LINE '78) 
(PUT 'GFABS 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFABS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFABS (X)
    (CONS (COND ((ATOM (CAR X)) (ABS (CAR X))) (T (|ABS:| (CAR X))))
          (COND ((ATOM (CDR X)) (ABS (CDR X))) (T (|ABS:| (CDR X)))))) 
(PUT 'GFABSTIM 'NUMBER-OF-ARGS 2) 
(PUT 'GFABSTIM 'DEFINED-ON-LINE '80) 
(PUT 'GFABSTIM 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFABSTIM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFABSTIM (B C)
    ((LAMBDA (BA BD CA CD)
       (PROGN
        (CONS
         (PLUBF (NORMBF (|ROUND:MT| (|TIMES:| BA CA) |:BPREC:|))
                (NORMBF (|ROUND:MT| (|TIMES:| BD CD) |:BPREC:|)))
         (PLUBF (NORMBF (|ROUND:MT| (|TIMES:| BA CD) |:BPREC:|))
                (NORMBF (|ROUND:MT| (|TIMES:| BD CA) |:BPREC:|))))))
     (CAR B) (CDR B) (CAR C) (CDR C))) 
(PUT 'RXRL 'NUMBER-OF-ARGS 2) 
(PUT 'RXRL 'DEFINED-ON-LINE '84) 
(PUT 'RXRL 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'RXRL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RXRL (P R)
    ((LAMBDA (C)
       (PROGN
        (SETQ R (COND ((ATOM R) (ABS R)) (T (|ABS:| R))))
        (PROG (I)
          (SETQ I (CDR P))
         LAB
          (COND ((NULL I) (RETURN NIL)))
          ((LAMBDA (I)
             (PROGN
              (SETQ C (NORMBF (|ROUND:MT| (|TIMES:| R C) |:BPREC:|)))
              (COND
               (I
                (SETQ C (PLUBF C (COND ((ATOM I) (ABS I)) (T (|ABS:| I)))))))))
           (CAR I))
          (SETQ I (CDR I))
          (GO LAB))
        C))
     ((LAMBDA (U) (COND ((ATOM U) (ABS U)) (T (|ABS:| U))))
      (CAR (SETQ P (CDR (NCOEFFS P))))))) 
(PUT 'CSEP 'NUMBER-OF-ARGS 1) 
(PUT 'CSEP 'DEFINED-ON-LINE '91) 
(PUT 'CSEP 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'CSEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CSEP (P)
    (PROGN
     (SETQ P (PRIMPC (MKINTEG P)))
     ((LAMBDA (G)
        (PROGN
         (COND
          ((NOT (ATOM G)) (PROGN (SETQ P (GFCPQUO P G)) (SETQ |PFACTOR#| T))))
         (CONS (N2GF G) P)))
      (GFGCD
       (PRIMCOEF
        (FILLZ
         (PROG (S FORALL-RESULT FORALL-ENDPTR)
           (SETQ S P)
           (COND ((NULL S) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (S) (CONS (CAR S) (CADR S))) (CAR S))
                                 NIL)))
          LOOPLABEL
           (SETQ S (CDR S))
           (COND ((NULL S) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (S) (CONS (CAR S) (CADR S))) (CAR S)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))
       (PRIMCOEF
        (FILLZ
         (PROG (S FORALL-RESULT FORALL-ENDPTR)
           (SETQ S P)
           (COND ((NULL S) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (S) (CONS (CAR S) (CDDR S))) (CAR S))
                                 NIL)))
          LOOPLABEL
           (SETQ S (CDR S))
           (COND ((NULL S) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (S) (CONS (CAR S) (CDDR S))) (CAR S)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))))) 
(PUT 'FILLZ 'NUMBER-OF-ARGS 1) 
(PUT 'FILLZ 'DEFINED-ON-LINE '99) 
(PUT 'FILLZ 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'FILLZ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FILLZ (P)
    (COND ((ATOM P) P)
          (T
           (PROG (C FORALL-RESULT FORALL-ENDPTR)
             (SETQ C (NCOEFFS P))
             (COND ((NULL C) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (C) (COND ((NULL C) 0) (T C))) (CAR C))
                              NIL)))
            LOOPLABEL
             (SETQ C (CDR C))
             (COND ((NULL C) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (C) (COND ((NULL C) 0) (T C))) (CAR C))
                           NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'PRIMCOEF 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMCOEF 'DEFINED-ON-LINE '102) 
(PUT 'PRIMCOEF 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'PRIMCOEF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMCOEF (P)
    (COND ((ATOM P) P)
          (T
           (PROG (D)
             (SETQ D 0)
             (SETQ P (REVERSE (CDR P)))
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND P (EQUAL (CAR P) 0))) (RETURN NIL)))
               (SETQ P (CDR P))
               (GO WHILELABEL))
             (COND ((NULL P) (RETURN 0)))
             (SETQ P (REVERSE P))
             (PROG ()
              WHILELABEL
               (COND ((NOT (AND P (EQUAL (CAR P) 0))) (RETURN NIL)))
               (SETQ P (CDR P))
               (GO WHILELABEL))
             (COND ((NULL (CDR P)) (RETURN 1)))
             (PROG (C)
               (SETQ C P)
              LAB
               (COND ((NULL C) (RETURN NIL)))
               ((LAMBDA (C) (SETQ D (GCDN D C))) (CAR C))
               (SETQ C (CDR C))
               (GO LAB))
             (RETURN
              ((LAMBDA (S)
                 (CONS (DIFFERENCE (LENGTH P) 1)
                       (PROG (C FORALL-RESULT FORALL-ENDPTR)
                         (SETQ C P)
                         (COND ((NULL C) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (C)
                                             (TIMES S (QUOTIENT C D)))
                                           (CAR C))
                                          NIL)))
                        LOOPLABEL
                         (SETQ C (CDR C))
                         (COND ((NULL C) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (C) (TIMES S (QUOTIENT C D)))
                                   (CAR C))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
               (SGN (CAR P)))))))) 
(PUT 'GFGCD 'NUMBER-OF-ARGS 2) 
(PUT 'GFGCD 'DEFINED-ON-LINE '114) 
(PUT 'GFGCD 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFGCD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFGCD (P Q)
    (COND ((OR (ATOM P) (ATOM Q)) 1) ((GREATERP (CAR Q) (CAR P)) (GFGCDR Q P))
          (T (GFGCDR P Q)))) 
(PUT 'GFGCDR 'NUMBER-OF-ARGS 2) 
(PUT 'GFGCDR 'DEFINED-ON-LINE '117) 
(PUT 'GFGCDR 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFGCDR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFGCDR (P1 P2)
    ((LAMBDA (R)
       (PROGN (COND ((EQUAL R 0) P2) ((ATOM R) 1) (T (GFGCDR P2 R)))))
     (PQREM P1 P2))) 
(PUT 'PQREM 'NUMBER-OF-ARGS 2) 
(PUT 'PQREM 'DEFINED-ON-LINE '121) 
(PUT 'PQREM 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'PQREM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PQREM (P1 P2)
    (PROG (A G N N2 M M1 M2)
      (SETQ N (DIFFERENCE (CAR P1) (SETQ N2 (CAR P2))))
      (SETQ P1 (CDR P1))
      (SETQ P2 (CDR P2))
      (SETQ M2 (CAR P2))
     LP
      (SETQ G (GCDN (SETQ M1 (CAR P1)) M2))
      (SETQ M1 (QUOTIENT M2 G))
      (SETQ A
              (SETQ P1
                      (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                        (SETQ Y P1)
                        (COND ((NULL Y) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (Y) (TIMES Y M1)) (CAR Y))
                                         NIL)))
                       LOOPLABEL
                        (SETQ Y (CDR Y))
                        (COND ((NULL Y) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (TIMES Y M1)) (CAR Y)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
      (SETQ M (QUOTIENT (CAR A) M2))
      (PROG (Y)
        (SETQ Y P2)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (RPLACA A (DIFFERENCE (CAR A) (TIMES Y M)))
            (SETQ A (CDR A))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (SETQ P1 (CDR P1))
      (COND ((GEQ (SETQ N (DIFFERENCE N 1)) 0) (GO LP)))
      (RETURN (PRIMCOEF (CONS (DIFFERENCE (LENGTH P1) 1) P1))))) 
(PUT 'NEGPREM 'NUMBER-OF-ARGS 2) 
(PUT 'NEGPREM 'DEFINED-ON-LINE '134) 
(PUT 'NEGPREM 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'NEGPREM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NEGPREM (P1 P2)
    (PROG (A G N N2 M M1 M2)
      (SETQ N (DIFFERENCE (CAR P1) (SETQ N2 (CAR P2))))
      (SETQ P1 (CDR P1))
      (SETQ P2 (CDR P2))
      (SETQ M2 (CAR P2))
     LP
      (SETQ G (GCDN (SETQ M1 (CAR P1)) M2))
      (SETQ M1 (ABS (QUOTIENT M2 G)))
      (SETQ P1
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y P1)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (TIMES Y M1)) (CAR Y))
                                      NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (TIMES Y M1)) (CAR Y)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ A P1)
      (SETQ M (QUOTIENT (CAR A) M2))
      (PROG (Y)
        (SETQ Y P2)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (RPLACA A (DIFFERENCE (CAR A) (TIMES Y M)))
            (SETQ A (CDR A))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (SETQ P1 (CDR P1))
      (COND ((GEQ (SETQ N (DIFFERENCE N 1)) 0) (GO LP)))
      (RETURN
       (PRIMPN
        (CONS (DIFFERENCE N2 1)
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y P1)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (MINUS Y)) (CAR Y)) NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (MINUS Y)) (CAR Y)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'GFCPQUO 'NUMBER-OF-ARGS 2) 
(PUT 'GFCPQUO 'DEFINED-ON-LINE '145) 
(PUT 'GFCPQUO 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFCPQUO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFCPQUO (P Q)
    (PROG (N C A D F Z PP)
      (SETQ Z (CONS 0 0))
      (SETQ N (DIFFERENCE (CAR (SETQ P (NCOEFFS P))) (CAR Q)))
      (SETQ PP
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R (CDR P))
                (COND ((NULL R) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (R) (COND (R R) (T Z))) (CAR R))
                                      NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (R) (COND (R R) (T Z))) (CAR R)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ C (CAR (SETQ Q (CDR Q))))
     LOOP
      (SETQ A (QUOTIENT (CAAR PP) C))
      (SETQ D (QUOTIENT (CDAR PP) C))
      (COND ((OR (NEQ A 0) (NEQ D 0)) (SETQ F (CONS (CONS N (CONS A D)) F))))
      (COND ((LESSP (SETQ N (DIFFERENCE N 1)) 0) (RETURN F)))
      (SETQ P PP)
      (PROG (R)
        (SETQ R Q)
       LAB
        (COND ((NULL R) (RETURN NIL)))
        ((LAMBDA (R)
           (PROGN
            (RPLACA P
                    (CONS (DIFFERENCE (CAAR P) (TIMES A R))
                          (DIFFERENCE (CDAR P) (TIMES D R))))
            (SETQ P (CDR P))))
         (CAR R))
        (SETQ R (CDR R))
        (GO LAB))
      (SETQ PP (CDR PP))
      (GO LOOP))) 
(PUT 'DEFLATE1 'NUMBER-OF-ARGS 2) 
(PUT 'DEFLATE1 'DEFINED-ON-LINE '159) 
(PUT 'DEFLATE1 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'DEFLATE1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEFLATE1 (P R)
    (PROG (Q N C)
      (SETQ N (CAR (SETQ P (NCOEFFS P))))
      (SETQ P (CDR P))
      (SETQ C (CAR P))
      (PROG (I)
        (SETQ I (CDR P))
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (PROGN
            (SETQ N (DIFFERENCE N 1))
            (COND ((NEQ (CADR C) 0) (SETQ Q (CONS (CONS N C) Q))))
            (SETQ C (|TIMES:| R C))
            (COND (I (SETQ C (|PLUS:| I C))))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN (CONS C Q)))) 
(PUT 'DEFLATE1C 'NUMBER-OF-ARGS 2) 
(PUT 'DEFLATE1C 'DEFINED-ON-LINE '169) 
(PUT 'DEFLATE1C 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'DEFLATE1C 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DEFLATE1C (P R)
    (PROG (Q N C)
      (SETQ N (CAR (SETQ P (NCOEFFS P))))
      (SETQ P (CDR P))
      (SETQ C (CAR P))
      (PROG (J)
        (SETQ J (CDR P))
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN
            (SETQ N (DIFFERENCE N 1))
            (COND
             ((NOT
               (COND
                ((NOT (ATOM (CAR C)))
                 (AND (EQUAL (CADR (CAR C)) 0) (EQUAL (CADR (CDR C)) 0)))
                (T (EQUAL C '(0.0 . 0.0)))))
              (SETQ Q (CONS (CONS N C) Q))))
            (SETQ C (GFTIMESN R C))
            (COND (J (SETQ C (GFPLUSN C J))))))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN (CONS C Q)))) 
(DE RL2GFC (X) (CONS X (COND ((ATOM X) 0.0) (T BFZ*)))) 
(PUT 'RL2GFC 'NUMBER-OF-ARGS 1) 
(PUT 'RL2GFC 'DEFINED-ON-LINE '179) 
(PUT 'RL2GFC 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'RL2GFC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'RL2GFC 'INLINE '(LAMBDA (X) (CONS X (COND ((ATOM X) 0.0) (T BFZ*))))) 
(PUT 'ACCUPR 'NUMBER-OF-ARGS 3) 
(PUT 'ACCUPR 'DEFINED-ON-LINE '182) 
(PUT 'ACCUPR 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'ACCUPR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ACCUPR (P Q R)
    (PROG (CQ CP RL *BFTAG S AC)
      (SETQ AC 0)
      (COND ((LESSP (CAAR (LASTPAIR Q)) 2) (RETURN 1)))
      (SETQ *BFTAG T)
      (SETQ R (GF2BF R))
      (SETQ CQ
              ((LAMBDA (P)
                 (NOT
                  (OR
                   (OR (NUMBERP P)
                       (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                   (OR (NUMBERP (CDAR P))
                       (AND (EQCAR (CDAR P) '|:RD:|)
                            (NOT (ATOM (CDR (CDAR P)))))))))
               (SETQ Q (BFLOATEM Q))))
      (SETQ CP
              (NOT
               (OR
                (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                (OR (NUMBERP (CDAR P))
                    (AND (EQCAR (CDAR P) '|:RD:|)
                         (NOT (ATOM (CDR (CDAR P)))))))))
      (SETQ RL
              (OR (OR (NUMBERP R) (AND (EQCAR R '|:RD:|) (NOT (ATOM (CDR R)))))
                  (AND
                   (COND ((ATOM (CDR R)) (ZEROP (CDR R)))
                         (T (EQUAL (CADR (CDR R)) 0)))
                   (SETQ R (CAR R)))))
      (SETQ Q
              (COND
               (RL
                (CDR
                 (COND
                  (CQ
                   (DEFLATE1C Q
                    (COND
                     (*BFTAG
                      (CONS
                       (COND ((FLOATP R) (FL2BF R))
                             (T
                              (NORMBF
                               (COND ((NOT (ATOM R)) R)
                                     ((FIXP R) (CONS '|:RD:| (CONS R 0)))
                                     (T (|READ:NUM| R))))))
                       BFZ*))
                     (T (CONS (CFLOT R) 0.0)))))
                  (T (DEFLATE1 Q R)))))
               ((NOT CQ) (DEFLATE2 Q R))
               (T
                (CDR
                 (COND (CP (DEFLATE1C Q R))
                       (T
                        (DEFLATE1C (CDR (DEFLATE1C Q R))
                         (CONS (CAR R) (|MINUS:| (CDR R))))))))))
      (COND ((GREATERP (CAAR Q) 0) (PROGN (SETQ AC |ACC#|) (GO RET))))
      (COND (RL (SETQ R (CONS R (COND ((ATOM R) 0.0) (T BFZ*))))))
      (SETQ P
              (BFSQRT
               ((LAMBDA (X)
                  (COND ((FLOATP X) (FL2BF X))
                        (T
                         (NORMBF
                          (COND ((NOT (ATOM X)) X)
                                ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                (T (|READ:NUM| X)))))))
                (GFRSQ R))))
      (SETQ P (|ROUND:DEC1| P (PLUS |ACC#| 2)))
      (SETQ P (PLUS 1 (CDR P) (LENGTH (EXPLODE (ABS (CAR P))))))
     LOOP
      (SETQ S
              (COND
               ((GREATERP (CAAR (LASTPAIR Q)) 1)
                ((LAMBDA (X)
                   (COND ((FLOATP X) (FL2BF X))
                         (T
                          (NORMBF
                           (COND ((NOT (ATOM X)) X)
                                 ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                                 (T (|READ:NUM| X)))))))
                 (MINBND1 Q R)))
               (T
                (BFSQRT
                 (GFRSQ
                  (GFPLUS R
                          (PROGN
                           (SETQ S (CDAR (BFPRIM Q)))
                           (COND
                            ((NOT CQ)
                             (COND
                              (*BFTAG
                               (CONS
                                (COND ((FLOATP S) (FL2BF S))
                                      (T
                                       (NORMBF
                                        (COND ((NOT (ATOM S)) S)
                                              ((FIXP S)
                                               (CONS '|:RD:| (CONS S 0)))
                                              (T (|READ:NUM| S))))))
                                BFZ*))
                              (T (CONS (CFLOT S) 0.0))))
                            (T S)))))))))
      (SETQ S (|ROUND:DEC1| S (PLUS |ACC#| 2)))
      (SETQ AC
              (MAX AC |ROOTACC#|
                   (DIFFERENCE (DIFFERENCE P (CDR S))
                               (LENGTH (EXPLODE (ABS (CAR S)))))))
      (COND
       ((AND CQ (NOT RL) (NOT CP))
        (PROGN
         (SETQ R (CONS (CAR R) (|MINUS:| (CDR R))))
         (SETQ RL T)
         (GO LOOP))))
     RET
      (COND (|ROOTACC##| (SETQ AC (MAX AC |ROOTACC##|))))
      (SETQ |ACCM#| (MAX AC |ACCM#|))
      (RETURN AC))) 
(PUT 'ORGSHIFT 'NUMBER-OF-ARGS 2) 
(PUT 'ORGSHIFT 'DEFINED-ON-LINE '212) 
(PUT 'ORGSHIFT 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'ORGSHIFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORGSHIFT (P ORG)
    (PROG (S CP N)
      (SETQ N 0)
      (COND
       (((LAMBDA (U)
           (COND
            ((NOT (ATOM (CAR U)))
             (AND (EQUAL (CADR (CAR U)) 0) (EQUAL (CADR (CDR U)) 0)))
            (T (EQUAL U '(0.0 . 0.0)))))
         (COND
          ((SETQ CP
                   (NOT
                    (OR
                     (OR (NUMBERP P)
                         (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                     (OR (NUMBERP (CDAR P))
                         (AND (EQCAR (CDAR P) '|:RD:|)
                              (NOT (ATOM (CDR (CDAR P)))))))))
           ORG)
          (T
           (COND
            (*BFTAG
             (CONS
              (COND ((FLOATP ORG) (FL2BF ORG))
                    (T
                     (NORMBF
                      (COND ((NOT (ATOM ORG)) ORG)
                            ((FIXP ORG) (CONS '|:RD:| (CONS ORG 0)))
                            (T (|READ:NUM| ORG))))))
              BFZ*))
            (T (CONS (CFLOT ORG) 0.0))))))
        (RETURN P)))
      (SETQ ORG (GF2BF ORG))
      (COND ((NUMBERP (LEADATOM (CDAR P))) (SETQ P (BFLOATEM P))))
      (COND
       (CP
        (PROG ()
         WHILELABEL
          (COND ((NOT P) (RETURN NIL)))
          (PROGN
           (SETQ P (DEFLATE1C P ORG))
           (COND
            ((NOT
              (COND
               ((NOT (ATOM (CAR (CAR P))))
                (AND (EQUAL (CADR (CAR (CAR P))) 0)
                     (EQUAL (CADR (CDR (CAR P))) 0)))
               (T (EQUAL (CAR P) '(0.0 . 0.0)))))
             (SETQ S (CONS (CONS N (CAR P)) S))))
           (SETQ N (PLUS N 1))
           (SETQ P (CDR P)))
          (GO WHILELABEL)))
       (T
        (PROG ()
         WHILELABEL
          (COND ((NOT P) (RETURN NIL)))
          (PROGN
           (SETQ P (DEFLATE1 P ORG))
           (COND
            ((NOT
              (COND ((ATOM (CAR P)) (ZEROP (CAR P)))
                    (T (EQUAL (CADR (CAR P)) 0))))
             (SETQ S (CONS (CONS N (CAR P)) S))))
           (SETQ N (PLUS N 1))
           (SETQ P (CDR P)))
          (GO WHILELABEL))))
      (RETURN (REVERSIP (COND (|PFLT#| (CFLOTEM S)) (T (BFRNDEM S))))))) 
(PUT 'BFRNDEM 'NUMBER-OF-ARGS 1) 
(PUT 'BFRNDEM 'DEFINED-ON-LINE '230) 
(PUT 'BFRNDEM 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'BFRNDEM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFRNDEM (S)
    ((LAMBDA (CP)
       (PROG (C FORALL-RESULT FORALL-ENDPTR)
         (SETQ C S)
         (COND ((NULL C) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (C)
                             (CONS (CAR C)
                                   (COND
                                    (CP
                                     (CONS
                                      (NORMBF (|ROUND:MT| (CADR C) |:BPREC:|))
                                      (NORMBF
                                       (|ROUND:MT| (CDDR C) |:BPREC:|))))
                                    (T
                                     (NORMBF
                                      (|ROUND:MT| (CDR C) |:BPREC:|))))))
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
                             (CONS (NORMBF (|ROUND:MT| (CADR C) |:BPREC:|))
                                   (NORMBF (|ROUND:MT| (CDDR C) |:BPREC:|))))
                            (T (NORMBF (|ROUND:MT| (CDR C) |:BPREC:|))))))
                   (CAR C))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))
     (NOT
      (OR (OR (NUMBERP S) (AND (EQCAR S '|:RD:|) (NOT (ATOM (CDR S)))))
          (OR (NUMBERP (CDAR S))
              (AND (EQCAR (CDAR S) '|:RD:|) (NOT (ATOM (CDR (CDAR S)))))))))) 
(PUT 'R2FLBF2R 'NUMBER-OF-ARGS 1) 
(PUT 'R2FLBF2R 'DEFINED-ON-LINE '235) 
(PUT 'R2FLBF2R 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'R2FLBF2R 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE R2FLBF2R (X) (REALRAT (R2FLBF X))) 
(PUT 'BFPRIM 'NUMBER-OF-ARGS 1) 
(PUT 'BFPRIM 'DEFINED-ON-LINE '237) 
(PUT 'BFPRIM 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'BFPRIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BFPRIM (P)
    ((LAMBDA (D)
       (PROGN
        (COND
         ((OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
              (OR (NUMBERP (CDAR P))
                  (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P)))))))
          (PROG (Y FORALL-RESULT FORALL-ENDPTR)
            (SETQ Y P)
            (COND ((NULL Y) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (Y) (CONS (CAR Y) (BFDIVIDE (CDR Y) D)))
                              (CAR Y))
                             NIL)))
           LOOPLABEL
            (SETQ Y (CDR Y))
            (COND ((NULL Y) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (Y) (CONS (CAR Y) (BFDIVIDE (CDR Y) D))) (CAR Y))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))
         (T
          (PROG (Y FORALL-RESULT FORALL-ENDPTR)
            (SETQ Y P)
            (COND ((NULL Y) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (Y)
                                (CONS (CAR Y) (GFQUOTIENT (CDR Y) D)))
                              (CAR Y))
                             NIL)))
           LOOPLABEL
            (SETQ Y (CDR Y))
            (COND ((NULL Y) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (Y) (CONS (CAR Y) (GFQUOTIENT (CDR Y) D)))
                      (CAR Y))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
     (CDAR (LASTPAIR P)))) 
(PUT 'PRIMPC 'NUMBER-OF-ARGS 1) 
(PUT 'PRIMPC 'DEFINED-ON-LINE '243) 
(PUT 'PRIMPC 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'PRIMPC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PRIMPC (P)
    (PROG (D)
      (SETQ D 0)
      (PROG (Y)
        (SETQ Y P)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (SETQ D (GCDN (CADR Y) (GCDN D (CDDR Y))))) (CAR Y))
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
                             (CONS (CAR Y)
                                   (CONS (QUOTIENT (CADR Y) D)
                                         (QUOTIENT (CDDR Y) D))))
                           (CAR Y))
                          NIL)))
        LOOPLABEL
         (SETQ Y (CDR Y))
         (COND ((NULL Y) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS
                  ((LAMBDA (Y)
                     (CONS (CAR Y)
                           (CONS (QUOTIENT (CADR Y) D) (QUOTIENT (CDDR Y) D))))
                   (CAR Y))
                  NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'UNGFFC 'NUMBER-OF-ARGS 1) 
(PUT 'UNGFFC 'DEFINED-ON-LINE '250) 
(PUT 'UNGFFC 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'UNGFFC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNGFFC (P)
    (PROG (R C)
      (SETQ C (GTAG (CADAR P)))
      (COND
       ((EQUAL (CAAR P) 0)
        (PROGN
         (COND
          ((NOT
            (COND
             ((NOT (ATOM (CAR (CDAR P))))
              (AND (EQUAL (CADR (CAR (CDAR P))) 0)
                   (EQUAL (CADR (CDR (CDAR P))) 0)))
             (T (EQUAL (CDAR P) '(0.0 . 0.0)))))
           (SETQ R (CONS C (CDAR P)))))
         (SETQ P (CDR P)))))
      (PROG (I)
        (SETQ I P)
       LAB
        (COND ((NULL I) (RETURN NIL)))
        ((LAMBDA (I)
           (COND
            ((NOT
              (COND
               ((NOT (ATOM (CAR (CDR I))))
                (AND (EQUAL (CADR (CAR (CDR I))) 0)
                     (EQUAL (CADR (CDR (CDR I))) 0)))
               (T (EQUAL (CDR I) '(0.0 . 0.0)))))
             (SETQ R (CONS (CONS (CONS *RVAR (CAR I)) (CONS C (CDR I))) R)))))
         (CAR I))
        (SETQ I (CDR I))
        (GO LAB))
      (RETURN R))) 
(PUT 'ISCALE 'NUMBER-OF-ARGS 2) 
(PUT 'ISCALE 'DEFINED-ON-LINE '260) 
(PUT 'ISCALE 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'ISCALE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ISCALE (D Y) (ASHIFT (CADR Y) (PLUS D (CDDR Y)))) 
(PUT 'MKINTEG 'NUMBER-OF-ARGS 1) 
(PUT 'MKINTEG 'DEFINED-ON-LINE '262) 
(PUT 'MKINTEG 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'MKINTEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKINTEG (P)
    ((LAMBDA (NC)
       (PROG (M)
         (SETQ M 0)
         (SETQ P (BFLOATEM P))
         (PROG (Y)
           (SETQ Y P)
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (SETQ M
                      (COND (NC (MAX M (MINUS (CDDR (CDR Y)))))
                            (T
                             (MAX M (MINUS (CDDR (CADR Y)))
                                  (MINUS (CDDR (CDDR Y))))))))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         (SETQ P
                 (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                   (SETQ Y P)
                   (COND ((NULL Y) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (Y)
                                       (CONS (CAR Y)
                                             (COND (NC (ISCALE M (CDR Y)))
                                                   (T
                                                    (CONS (ISCALE M (CADR Y))
                                                          (ISCALE M
                                                           (CDDR Y)))))))
                                     (CAR Y))
                                    NIL)))
                  LOOPLABEL
                   (SETQ Y (CDR Y))
                   (COND ((NULL Y) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (Y)
                               (CONS (CAR Y)
                                     (COND (NC (ISCALE M (CDR Y)))
                                           (T
                                            (CONS (ISCALE M (CADR Y))
                                                  (ISCALE M (CDDR Y)))))))
                             (CAR Y))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (RETURN (COND (NC (PRIMP P)) (T (PRIMPC P))))))
     (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
         (OR (NUMBERP (CDAR P))
             (AND (EQCAR (CDAR P) '|:RD:|) (NOT (ATOM (CDR (CDAR P))))))))) 
(PUT 'MKGIRAT 'NUMBER-OF-ARGS 1) 
(PUT 'MKGIRAT 'DEFINED-ON-LINE '276) 
(PUT 'MKGIRAT 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'MKGIRAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKGIRAT (J)
    (PROG (RA RD IA ID RO IO)
      (COND
       ((EQCAR J '|:DN:|)
        (PROGN
         (SETQ RA (CADR J))
         (SETQ RO (CDDR J))
         (SETQ RD 1)
         (COND ((LESSP RO 0) (SETQ RD (EXPT 10 (MINUS RO))))
               ((GREATERP RO 0) (SETQ RA (EXPT 10 RO))))
         (RETURN (CONS (MKRN RA RD) 1))))
       ((AND (PAIRP J) (EQCAR (CAR J) '|:DN:|))
        (PROGN
         (SETQ RA (CADAR J))
         (SETQ RO (CDDAR J))
         (SETQ RD 1)
         (COND ((LESSP RO 0) (SETQ RD (EXPT 10 (MINUS RO))))
               ((GREATERP RO 0) (SETQ RA (EXPT 10 RO))))
         (SETQ IA (CADDR J))
         (SETQ IO (CDDDR J))
         (SETQ ID 1)
         (COND ((LESSP IO 0) (SETQ ID (EXPT 10 (MINUS IO))))
               ((GREATERP IO 0) (SETQ IA (EXPT 10 IO))))
         (SETQ RA (CAR (SETQ RD (CDR (MKRN RA RD)))))
         (SETQ RD (CDR RD))
         (SETQ IA (CAR (SETQ ID (CDR (MKRN IA ID)))))
         (SETQ ID (CDR ID))
         (GO LCM))))
      (COND
       (((LAMBDA (P)
           (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P))))))
         (SETQ J (GF2BF J)))
        (RETURN
         (CDR
          (*RD2RN
           (ROOTRND
            (COND ((FLOATP J) (FL2BF J))
                  (T
                   (NORMBF
                    (COND ((NOT (ATOM J)) J)
                          ((FIXP J) (CONS '|:RD:| (CONS J 0)))
                          (T (|READ:NUM| J))))))))))))
      (SETQ J (GFRTRND (GF2BF J)))
      (SETQ RA (CAR (SETQ RD (CDR (*RD2RN (CAR J))))))
      (SETQ RD (CDR RD))
      (SETQ IA (CAR (SETQ ID (CDR (*RD2RN (CDR J))))))
      (SETQ ID (CDR ID))
     LCM
      (SETQ J (TIMES (QUOTIENT ID (GCDN ID RD)) RD))
      (SETQ RO (QUOTIENT J RD))
      (SETQ IO (QUOTIENT J ID))
      (RETURN (CONS (CONS '|:GI:| (CONS (TIMES RA RO) (TIMES IA IO))) J)))) 
(PUT 'MKPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'MKPOLY 'DEFINED-ON-LINE '298) 
(PUT 'MKPOLY 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'MKPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKPOLY (RTL)
    (COND ((EQCAR RTL 'LIST) (NUM (MKPOLY1 (CDR RTL))))
          (T
           (TYPERR (COND ((EQCAR RTL '*SQ) (PREPSQ (CADR RTL))) (T RTL))
                   "list")))) 
(PUT 'MKPOLY1 'NUMBER-OF-ARGS 1) 
(PUT 'MKPOLY1 'DEFINED-ON-LINE '303) 
(PUT 'MKPOLY1 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'MKPOLY1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKPOLY1 (R)
    (COND ((NULL (CDR R)) (MKDIFFER (CAR R)))
          (T (CONS 'TIMES (LIST (MKDIFFER (CAR R)) (MKPOLY1 (CDR R))))))) 
(PUT 'GETROOT 'NUMBER-OF-ARGS 2) 
(PUT 'GETROOT 'DEFINED-ON-LINE '307) 
(PUT 'GETROOT 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GETROOT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETROOT (N R)
    (COND
     ((OR (LESSP (SETQ N (FIX N)) 1) (GREATERP N (LENGTH (SETQ R (CDR R)))))
      (RERROR 'ROOTS 4 "n out of range"))
     (T
      (PROGN
       (PROG ()
        WHILELABEL
         (COND ((NOT (GREATERP (SETQ N (DIFFERENCE N 1)) 0)) (RETURN NIL)))
         (SETQ R (CDR R))
         (GO WHILELABEL))
       (CADDAR R))))) 
(PUT 'MKDIFFER 'NUMBER-OF-ARGS 1) 
(PUT 'MKDIFFER 'DEFINED-ON-LINE '312) 
(PUT 'MKDIFFER 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'MKDIFFER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKDIFFER (R) (CONS 'DIFFERENCE (CDR R))) 
(FLAG '(MKPOLY GETROOT) 'OPFN) 
(PUT 'A2GF 'NUMBER-OF-ARGS 1) 
(PUT 'A2GF 'DEFINED-ON-LINE '316) 
(PUT 'A2GF 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'A2GF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE A2GF (X)
    (BFIXUP
     (COND
      ((OR (NUMBERP X) (AND (EQCAR X '|:RD:|) (NOT (ATOM (CDR X)))))
       (COND
        (*BFTAG
         (CONS
          (COND ((FLOATP X) (FL2BF X))
                (T
                 (NORMBF
                  (COND ((NOT (ATOM X)) X) ((FIXP X) (CONS '|:RD:| (CONS X 0)))
                        (T (|READ:NUM| X))))))
          BFZ*))
        (T (CONS (CFLOT X) 0.0))))
      ((AND (NOT (ATOM X))
            (OR (NUMBERP (CAR X))
                (AND (EQCAR (CAR X) '|:RD:|) (NOT (ATOM (CDR (CAR X)))))))
       (CONS (R2FLBF (CAR X)) (R2FLBF (CDR X))))
      (T
       (PROGN
        ((LAMBDA (Y)
           (COND
            ((OR (ERRORP Y) (NULL (SETQ Y (CAR Y))))
             (ERROR 0 (LIST X "is illegal as root parameter")))
            (T Y)))
         (ERRORSET* (LIST 'A2GF1 (MKQUOTE X)) NIL))))))) 
(PUT 'A2GF1 'NUMBER-OF-ARGS 1) 
(PUT 'A2GF1 'DEFINED-ON-LINE '325) 
(PUT 'A2GF1 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'A2GF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE A2GF1 (X)
    ((LAMBDA (D)
       (PROGN
        (SETQ X (CAR D))
        (SETQ D (CDR D))
        (COND
         ((OR (ATOM X) (ATOM (CAR X)))
          (COND
           ((EQCAR X '|:GI:|)
            (COND ((ATOM D) (GFQUOTBF (CADR X) (CDDR X) D))
                  ((EQCAR D '|:GI:|) (GFQUOTIENT (GI2GF X) (GI2GF D)))
                  (T NIL)))
           (((LAMBDA (P)
               (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P))))))
             (SETQ X (GFSIMP X)))
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
             (BFDIVIDE (R2FLBF X) (R2FLBF (GFSIMP D)))))
           (T X)))
         ((EQUAL (CAAR X) '(I . 1)) (GFQUOTBF (CDR X) (CDAR X) D)))))
     (SIMP* X))) 
(PUT 'GI2GF 'NUMBER-OF-ARGS 1) 
(PUT 'GI2GF 'DEFINED-ON-LINE '337) 
(PUT 'GI2GF 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GI2GF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GI2GF (X)
    (CONS
     (COND ((FLOATP (CADR X)) (FL2BF (CADR X)))
           (T
            (NORMBF
             (COND ((NOT (ATOM (CADR X))) (CADR X))
                   ((FIXP (CADR X)) (CONS '|:RD:| (CONS (CADR X) 0)))
                   (T (|READ:NUM| (CADR X)))))))
     (COND ((FLOATP (CDDR X)) (FL2BF (CDDR X)))
           (T
            (NORMBF
             (COND ((NOT (ATOM (CDDR X))) (CDDR X))
                   ((FIXP (CDDR X)) (CONS '|:RD:| (CONS (CDDR X) 0)))
                   (T (|READ:NUM| (CDDR X))))))))) 
(PUT 'GFQUOTBF 'NUMBER-OF-ARGS 3) 
(PUT 'GFQUOTBF 'DEFINED-ON-LINE '339) 
(PUT 'GFQUOTBF 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFQUOTBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GFQUOTBF (RL IM D) (CONS (COND (RL (QUOTBF RL D)) (T BFZ*)) (QUOTBF IM D))) 
(PUT 'QUOTBF 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTBF 'DEFINED-ON-LINE '342) 
(PUT 'QUOTBF 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'QUOTBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTBF (N D)
    (PROGN
     (COND ((EQCAR N '|:RN:|) (PROGN (SETQ D (CDDR N)) (SETQ N (CADR N))))
           (T (SETQ N (FTOUT N))))
     (NORMBF
      (|DIVIDE:|
       (COND ((FLOATP N) (FL2BF N))
             (T
              (NORMBF
               (COND ((NOT (ATOM N)) N) ((FIXP N) (CONS '|:RD:| (CONS N 0)))
                     (T (|READ:NUM| N))))))
       (COND ((FLOATP D) (FL2BF D))
             (T
              (NORMBF
               (COND ((NOT (ATOM D)) D) ((FIXP D) (CONS '|:RD:| (CONS D 0)))
                     (T (|READ:NUM| D))))))
       |:BPREC:|)))) 
(PUT 'STURM1 'NUMBER-OF-ARGS 1) 
(PUT 'STURM1 'DEFINED-ON-LINE '346) 
(PUT 'STURM1 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'STURM1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STURM1 (P)
    (PROG (B C S)
      (SETQ B (FILLZ (PRIMP (INTDIFF (SETQ P (MKINTEG P))))))
      (SETQ S (LIST B (SETQ P (SETQ *INTP (FILLZ P)))))
      (COND
       ((NOT (ATOM B))
        (PROG ()
         REPEATLABEL
          (PROGN
           (SETQ C (NEGPREM P B))
           (SETQ S (CONS C S))
           (SETQ P B)
           (SETQ B C))
          (COND ((NOT (ATOM C)) (GO REPEATLABEL))))))
      (SETQ *MULTRT (EQUAL C 0))
      (RETURN (SETQ *STRM (REVERSE S))))) 
(PUT 'GFSHIFT 'NUMBER-OF-ARGS 1) 
(PUT 'GFSHIFT 'DEFINED-ON-LINE '357) 
(PUT 'GFSHIFT 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFSHIFT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFSHIFT (P)
    (PROG (PR N ORG)
      (SETQ |SPREC#| (CONS '|:RD:| (CONS 3 (DIFFERENCE 1 |:BPREC:|))))
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
      (COND ((NULL P) (RETURN *XO)))
      (SETQ N (CAR (SETQ PR (NCOEFFS (BFPRIM P)))))
      (COND ((GREATERP N 1) (SETQ PR (CADDR PR))))
      (COND
       (PR
        (COND
         ((NOT
           (OR (OR (NUMBERP P) (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
               (OR (NUMBERP (CDAR P))
                   (AND (EQCAR (CDAR P) '|:RD:|)
                        (NOT (ATOM (CDR (CDAR P))))))))
          (SETQ *XO
                  (SETQ ORG
                          (GFQUOTIENT PR
                                      ((LAMBDA (U)
                                         (COND
                                          (*BFTAG
                                           (CONS
                                            (COND ((FLOATP U) (FL2BF U))
                                                  (T
                                                   (NORMBF
                                                    (COND ((NOT (ATOM U)) U)
                                                          ((FIXP U)
                                                           (CONS '|:RD:|
                                                                 (CONS U 0)))
                                                          (T
                                                           (|READ:NUM| U))))))
                                            BFZ*))
                                          (T (CONS (CFLOT U) 0.0))))
                                       (MINUS N))))))
         (T
          (SETQ *XO
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
                   (SETQ ORG (BFDIVIDE PR (R2FLBF (MINUS N))))))))))
      (COND ((NULL PR) (RETURN P)))
      (RETURN ((LAMBDA (|PFLT#|) (ORGSHIFT P ORG)) (NULL *BFTAG))))) 
(PUT 'P1RMULT 'NUMBER-OF-ARGS 1) 
(PUT 'P1RMULT 'DEFINED-ON-LINE '367) 
(PUT 'P1RMULT 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'P1RMULT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE P1RMULT (P) (AUTOMOD (CAR (SIMP* (P1RMULT1 P))))) 
(PUT 'P1RMULT1 'NUMBER-OF-ARGS 1) 
(PUT 'P1RMULT1 'DEFINED-ON-LINE '369) 
(PUT 'P1RMULT1 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'P1RMULT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE P1RMULT1 (P)
    (COND ((ATOM P) NIL) ((ATOM (CDR P)) (REVAL1 (MK*SQ (CONS (CAAR P) 1)) T))
          (T (LIST 'TIMES (P1RMULT1 (LIST (CAR P))) (P1RMULT1 (CDR P)))))) 
(PUT 'XOSHIFT 'NUMBER-OF-ARGS 2) 
(PUT 'XOSHIFT 'DEFINED-ON-LINE '374) 
(PUT 'XOSHIFT 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'XOSHIFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XOSHIFT (P NX)
    (PROG (N ORG PR CP ORGC A B)
      (SETQ N (CAR (SETQ PR (NCOEFFS (BFPRIM P)))))
      (COND ((GREATERP N 1) (SETQ PR (CADDR PR))))
      (COND ((NULL PR) (RETURN NIL)))
      (SETQ ORG
              (COND
               ((SETQ CP
                        (NOT
                         (OR
                          (OR (NUMBERP P)
                              (AND (EQCAR P '|:RD:|) (NOT (ATOM (CDR P)))))
                          (OR (NUMBERP (CDAR P))
                              (AND (EQCAR (CDAR P) '|:RD:|)
                                   (NOT (ATOM (CDR (CDAR P)))))))))
                (GFQUOTIENT PR
                            ((LAMBDA (U)
                               (COND
                                (*BFTAG
                                 (CONS
                                  (COND ((FLOATP U) (FL2BF U))
                                        (T
                                         (NORMBF
                                          (COND ((NOT (ATOM U)) U)
                                                ((FIXP U)
                                                 (CONS '|:RD:| (CONS U 0)))
                                                (T (|READ:NUM| U))))))
                                  BFZ*))
                                (T (CONS (CFLOT U) 0.0))))
                             (MINUS N))))
               (T (BFDIVIDE PR (R2FLBF (MINUS N))))))
      (SETQ ORGC
              (COND (CP ORG)
                    (T
                     (COND
                      (*BFTAG
                       (CONS
                        (COND ((FLOATP ORG) (FL2BF ORG))
                              (T
                               (NORMBF
                                (COND ((NOT (ATOM ORG)) ORG)
                                      ((FIXP ORG) (CONS '|:RD:| (CONS ORG 0)))
                                      (T (|READ:NUM| ORG))))))
                        BFZ*))
                      (T (CONS (CFLOT ORG) 0.0))))))
      (COND
       ((ERRORP
         (SETQ B
                 (ERRORSET*
                  (LIST 'GFRSQ (LIST 'GFVAL (MKQUOTE P) (MKQUOTE ORGC))) NIL)))
        (RETURN (BFLESSP (GFRSQ (GF2BF ORGC)) BFONE*)))
       (T (SETQ B (CAR B))))
      (SETQ A
              (GFRSQ
               (GFVAL P
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
      (COND
       ((AND *ROOTMSG *TRROOT)
        (PROGN (PROGN (PRIN2 (LIST "a=" A " b=" B)) NIL) (TERPRI))))
      (RETURN (NOT (BFLESSP A B))))) 
(PUT 'GFFINITR 'NUMBER-OF-ARGS 1) 
(PUT 'GFFINITR 'DEFINED-ON-LINE '390) 
(PUT 'GFFINITR 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFFINITR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GFFINITR (P) ((LAMBDA (*BFTAG) (GFFINIT P)) *BFTAG)) 
(PUT 'INVPOLY 'NUMBER-OF-ARGS 1) 
(PUT 'INVPOLY 'DEFINED-ON-LINE '394) 
(PUT 'INVPOLY 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'INVPOLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVPOLY (P)
    (PROGN
     (SETQ P
             (NCOEFFS
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R P)
                (COND ((NULL R) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (R)
                                    (CONS (DIFFERENCE (CAR R) (CAAR P))
                                          (CDR R)))
                                  (CAR R))
                                 NIL)))
               LOOPLABEL
                (SETQ R (CDR R))
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (R)
                            (CONS (DIFFERENCE (CAR R) (CAAR P)) (CDR R)))
                          (CAR R))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))
     (N2GF (CONS (CAR P) (REVERSIP (CDR P)))))) 
(PUT 'BDSTEST 'NUMBER-OF-ARGS 1) 
(PUT 'BDSTEST 'DEFINED-ON-LINE '399) 
(PUT 'BDSTEST 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'BDSTEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BDSTEST (I)
    (PROG (Y)
      (COND
       ((|EQUAL:| (ROOTRND (R2BF (CAR I))) (SETQ Y (ROOTRND (R2BF (CDR I)))))
        (RETURN Y))))) 
(PUT 'RLRTNO2 'NUMBER-OF-ARGS 1) 
(PUT 'RLRTNO2 'DEFINED-ON-LINE '404) 
(PUT 'RLRTNO2 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'RLRTNO2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RLRTNO2 (P)
    (COND ((NULL (STURM P)) 0)
          ((NULL |LIMS#|) (DIFFERENCE (SCHINF (MINUS 1)) (SCHINF 1)))
          (T
           (PROG (A B)
             (SETQ A (CAR |LIMS#|))
             (COND
              ((NULL (CDR |LIMS#|))
               (RETURN
                (COND
                 ((LESSP A 0)
                  (DIFFERENCE (DIFFERENCE (SCHINF (MINUS 1)) (SCHINF 0))
                              (ADJST (REALRAT 0))))
                 (T (DIFFERENCE (SCHINF 0) (SCHINF 1)))))))
             (RETURN
              (COND
               ((EQUAL (SETQ B (CADR |LIMS#|)) 'INFTY)
                (DIFFERENCE (SCHXA A) (SCHINF 1)))
               ((EQUAL A 'MINFTY) (DIFFERENCE (SCHINF (MINUS 1)) (SCHXB B)))
               (T (DIFFERENCE (SCHXA A) (SCHXB B))))))))) 
(PUT 'SCHXA 'NUMBER-OF-ARGS 1) 
(PUT 'SCHXA 'DEFINED-ON-LINE '415) 
(PUT 'SCHXA 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'SCHXA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SCHXA (A)
    (COND ((EQCAR A 'LIST) (SCH (CDR A))) (T (PLUS (SCH A) (ADJST A))))) 
(PUT 'SCHXB 'NUMBER-OF-ARGS 1) 
(PUT 'SCHXB 'DEFINED-ON-LINE '417) 
(PUT 'SCHXB 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'SCHXB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SCHXB (B)
    (COND ((EQCAR B 'LIST) (PLUS (SCH (CDR B)) (ADJST (CDR B)))) (T (SCH B)))) 
(PUT 'ADJST 'NUMBER-OF-ARGS 1) 
(PUT 'ADJST 'DEFINED-ON-LINE '420) 
(PUT 'ADJST 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'ADJST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADJST (L) (COND ((EQUAL (SGN1 (CAR *STRM) L) 0) 1) (T 0))) 
(PUT 'LIMADJ 'NUMBER-OF-ARGS 1) 
(PUT 'LIMADJ 'DEFINED-ON-LINE '422) 
(PUT 'LIMADJ 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'LIMADJ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LIMADJ (M)
    (COND ((NOT M) |LIMS#|)
          ((LESSP (LENGTH |LIMS#|) 2)
           (COND ((EQUAL (REMAINDER M 2) 0) (LIST 1)) (T NIL)))
          (T
           ((LAMBDA (A B)
              (COND ((RATLESSP (LVAL B) (LVAL A)) (LIST B A)) (T (LIST A B))))
            (LPWR (CAR |LIMS#|) M) (LPWR (CADR |LIMS#|) M))))) 
(PUT 'GFSTORVAL 'NUMBER-OF-ARGS 2) 
(PUT 'GFSTORVAL 'DEFINED-ON-LINE '429) 
(PUT 'GFSTORVAL 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFSTORVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GFSTORVAL (PF XN) (SETQ *XNLIST (CONS (CONS PF XN) *XNLIST))) 
(PUT 'GFGETMIN 'NUMBER-OF-ARGS 0) 
(PUT 'GFGETMIN 'DEFINED-ON-LINE '431) 
(PUT 'GFGETMIN 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'GFGETMIN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GFGETMIN NIL
    (PROG (Y NX L)
      (SETQ L *XNLIST)
      (SETQ NX (CAR (SETQ Y (CAR L))))
      (PROG (X)
        (SETQ X (CDR L))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (COND ((BFLESSP (CAR X) NX) (SETQ NX (CAR (SETQ Y X))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CDR Y)))) 
(PUT 'CALCPREC 'NUMBER-OF-ARGS 5) 
(PUT 'CALCPREC 'DEFINED-ON-LINE '437) 
(PUT 'CALCPREC 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'CALCPREC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CALCPREC (M N R AV S2)
    (PROG (P)
      (SETQ P 0)
      (SETQ P
              (COND ((LESSP M 2) (PLUS 1 (MAX |ACC#| N)))
                    (T
                     (MAX (PLUS |ACC#| 1)
                          (PLUS N 1 (CEILING (LOG10 (FLOAT M)))
                                (COND ((EQUAL R 1) 0)
                                      (T
                                       (MAX
                                        (COND
                                         ((OR (GREATERP S2 2.2) (LESSP S2 1.0))
                                          0)
                                         ((GREATERP S2 1.7) 2) (T 3))
                                        (COND
                                         ((AND (GREATERP M 3)
                                               (GREATERP (TIMES 1.5 AV)
                                                         (PLUS N 1)))
                                          (FIX
                                           (PLUS
                                            (TIMES 0.7 (MAX |ACC#| 7)
                                                   (LOG (FLOAT M)))
                                            0.5)))
                                         (T 0))))))))))
      (COND
       ((AND *ROOTMSG *TRROOT)
        (PROGN
         (PROGN
          (PRIN2
           (LIST "m=" M " n=" N " a=" |ACC#| " r=" R " av=" AV " s2=" S2 "->"
                 P))
          NIL)
         (TERPRI))))
      (RETURN P))) 
(PUT 'RRPWR 'NUMBER-OF-ARGS 2) 
(PUT 'RRPWR 'DEFINED-ON-LINE '449) 
(PUT 'RRPWR 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'RRPWR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RRPWR (R M)
    ((LAMBDA (RR)
       (PROGN
        (PROG ()
         WHILELABEL
          (COND ((NOT (GREATERP (SETQ M (DIFFERENCE M 1)) 0)) (RETURN NIL)))
          (SETQ RR (GFTIMES RR R))
          (GO WHILELABEL))
        RR))
     (SETQ R (A2GF R)))) 
(PUT 'CVT2 'NUMBER-OF-ARGS 2) 
(PUT 'CVT2 'DEFINED-ON-LINE '453) 
(PUT 'CVT2 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'CVT2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CVT2 (A B)
    (AND (NEQ (CADR A) 0) (NEQ (CADR B) 0)
         (PROGN
          (SETQ A (|DIVIDE:| (|ROUND:MT| A 20) (|ROUND:MT| B 20) 16))
          (AND (|LESSP:| A BFONE*)
               (|GREATERP:| A (DECIMAL2INTERNAL 1 (MINUS 1))))))) 
(PUT 'DSPLY 'NUMBER-OF-ARGS 1) 
(PUT 'DSPLY 'DEFINED-ON-LINE '457) 
(PUT 'DSPLY 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'DSPLY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DSPLY (NX)
    (COND
     (*ROOTMSG
      (PROGN
       ((LAMBDA (N)
          (PROGN
           (PROGN (PRIN2 "  prec is ") (PRIN2 (PLUS 2 (PRECISION 0))) NIL)
           (TERPRI)
           (PRINT_THE_NUMBER NX)
           (TERPRI)
           (WRS N)))
        (WRS NIL)))))) 
(PUT 'LEADATOM 'NUMBER-OF-ARGS 1) 
(PUT 'LEADATOM 'DEFINED-ON-LINE '461) 
(PUT 'LEADATOM 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'LEADATOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEADATOM (X) (COND ((ATOM X) X) (T (LEADATOM (CAR X))))) 
(PUT 'CVT5 'NUMBER-OF-ARGS 2) 
(PUT 'CVT5 'DEFINED-ON-LINE '463) 
(PUT 'CVT5 'DEFINED-IN-FILE 'ROOTS/COMPLXP.RED) 
(PUT 'CVT5 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CVT5 (A B) (|EQUAL:| (|ROUND:MT| A 20) (|ROUND:MT| B 20))) 
(ENDMODULE) 