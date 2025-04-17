(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'HILBERTP)) 
(PUT 'NEWHILBI 'NUMBER-OF-ARGS 3) 
(PUT 'NEWHILBI 'DEFINED-ON-LINE '28) 
(PUT 'NEWHILBI 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'NEWHILBI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NEWHILBI (BAS VAR VARS)
    (PROG (BASLT N U GRAD H JOA A II DIM0 VARX DMODE* *MODULAR)
      (SETQ BASLT
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (CDR BAS))
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ U (HGSPLITEVAL (LIST P VARS)))
                                     (CADR U)))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROGN
                             (SETQ U (HGSPLITEVAL (LIST P VARS)))
                             (CADR U)))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (X)
        (SETQ X (CDR VARS))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((PAIRP X)
             (SETQ BASLT
                     (CDR
                      (SUBEVAL
                       (LIST (LIST 'EQUAL X (GENSYM)) (CONS 'LIST BASLT))))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ VARX (*Q2F (SIMP* VAR)))
      (COND
       ((NOT
         (AND (EQUAL (CDAAR VARX) 1) (EQUAL (CDAR VARX) 1) (NULL (CDR VARX))))
        (PROGN
         (TERPRI)
         (PRIN2 "***** a value of >")
         (PRIN2 VAR)
         (PRIN2 "< has been set;")
         (TERPRI)
         (SETQ VAR (GENSYM))
         (SETQ VARX (*Q2F (SIMP* VAR)))
         (PRIN2 "***** >")
         (PRIN2 VAR)
         (PRIN2 "< is selected as variable.")
         (TERPRI))))
      (SETQ JOA (HILBSEREVAL (LIST (CONS 'LIST BASLT) VAR)))
      (SETQ GRAD (DEG JOA VAR))
      (SETQ A
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
                (COND ((MINUSP (DIFFERENCE GRAD I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (COEFFN JOA VAR I) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE GRAD I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (COEFFN JOA VAR I) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ N (LENGTH (CDR VARS)))
      (SETQ DIM0 1)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
        (SETQ DIM0
                ((LAMBDA (G133)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G133 DIM0))
                         (T (POLY-MULTF G133 DIM0))))
                 (ADDD I VARX)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ DIM0
              (MULTSQ (CONS DIM0 1)
                      (CONS 1
                            (PROG (I FORALL-RESULT)
                              (SETQ I 1)
                              (SETQ FORALL-RESULT 1)
                             LAB1
                              (COND
                               ((MINUSP (DIFFERENCE N I))
                                (RETURN FORALL-RESULT)))
                              (SETQ FORALL-RESULT (TIMES I FORALL-RESULT))
                              (SETQ I (PLUS2 I 1))
                              (GO LAB1)))))
      (SETQ H (MULTSQ (CONS (CAR A) 1) DIM0))
      (SETQ A (CDR A))
      (SETQ II 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT A) (RETURN NIL)))
        (PROGN
         (SETQ DIM0
                 (MULTSQ DIM0
                         (CONS (ADDF VARX (CAR (SIMP (MINUS II))))
                               (ADDF VARX (CAR (SIMP (DIFFERENCE N II)))))))
         (SETQ II (PLUS II 1))
         (COND
          ((NOT (EQUAL (CAR A) 0))
           (SETQ H (ADDSQ H (MULTSQ (CONS (CAR A) 1) DIM0)))))
         (SETQ A (CDR A)))
        (GO WHILELABEL))
      (RETURN (MK*SQ H)))) 
(PUT 'PSNEWHILBI 'NUMBER-OF-ARGS 1) 
(PUT 'PSNEWHILBI 'DEFINED-ON-LINE '68) 
(PUT 'PSNEWHILBI 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'PSNEWHILBI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PSNEWHILBI (U)
    (PROG (ZZ PL VL)
      (SETQ PL (REVAL1 (CAR U) T))
      (COND ((CDR U) (SETQ VL (LISTEVAL (CADR U) NIL))))
      (SETQ ZZ (CONS 'LIST (GROEBNERVARS (CDR PL) VL)))
      (RETURN (NEWHILBI PL 'X ZZ)))) 
(PUT 'HILBERTPOLYNOMIAL 'PSOPFN 'PSNEWHILBI) 
(PUT 'HGSPLITEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'HGSPLITEVAL 'DEFINED-ON-LINE '76) 
(PUT 'HGSPLITEVAL 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'HGSPLITEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HGSPLITEVAL (PARS)
    (PROG (VARS X U V W OLDORDER *FACTOR *EXP N PCOUNT*)
      (SETQ N 0)
      (SETQ PCOUNT* 0)
      (SETQ *EXP T)
      (SETQ N (LENGTH PARS))
      (SETQ U (REVAL1 (CAR PARS) T))
      (SETQ V (COND ((GREATERP N 1) (REVAL1 (CADR PARS) T)) (T NIL)))
      (SETQ U (LIST 'LIST U))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GROEREVLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (GROEBNERVARS W V))
      (COND ((NOT VARS) (VDPERR 'HILBERTPOLYNOMIAL)))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ W (A2VDP (CAR W)))
      (COND ((OR (NULL W) (NULL (CADR (CDDR W)))) (SETQ X W))
            (T
             (PROGN (SETQ X (VDPFMON '(1 . 1) (CADR W))) (SETQ W (VDPRED W)))))
      (SETQ W (LIST 'LIST (DIP2A (CADR (CDDR X))) (DIP2A (CADR (CDDR W)))))
      (SETKORDER OLDORDER)
      (RETURN W))) 
(PUT 'FUNCTIONINDEX2 'DEFINED-ON-LINE '107) 
(PUT 'FUNCTIONINDEX2 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'FUNCTIONINDEX2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DM FUNCTIONINDEX2 (U)
    (PROG (DIMS IND1 IND2)
      (SETQ DIMS (CADR U))
      (SETQ IND1 (CADDR U))
      (SETQ IND2 (CADDDR U))
      (RETURN
       (LIST 'IPLUS2 IND2
             (LIST 'ITIMES2 (LIST 'CADR DIMS) (LIST 'IPLUS2 IND1 (MINUS 1))))))) 
(PUT 'GETRARRAY 'DEFINED-ON-LINE '114) 
(PUT 'GETRARRAY 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'GETRARRAY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DM GETRARRAY (U)
    (PROG (ARRY INDS)
      (SETQ ARRY (CADR U))
      (SETQ INDS (CDDR U))
      (COND
       ((EQUAL (LENGTH INDS) 1)
        (RETURN (LIST 'GETV (LIST 'CDR ARRY) (CAR INDS))))
       (T
        (RETURN
         (LIST 'GETV (LIST 'CDR ARRY)
               (CONS 'FUNCTIONINDEX2 (CONS (LIST 'CAR ARRY) INDS)))))))) 
(PUT 'MAKERARRAY 'NUMBER-OF-ARGS 1) 
(PUT 'MAKERARRAY 'DEFINED-ON-LINE '122) 
(PUT 'MAKERARRAY 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'MAKERARRAY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKERARRAY (DIMS)
    (PROG (U N)
      (SETQ N
              (PROG (I FORALL-RESULT)
                (SETQ I DIMS)
                (SETQ FORALL-RESULT 1)
               LAB1
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (TIMES ((LAMBDA (I) I) (CAR I)) FORALL-RESULT))
                (SETQ I (CDR I))
                (GO LAB1)))
      (SETQ U (MKVECT N))
      (RETURN (CONS DIMS U)))) 
(PUT 'PUTRARRAY 'DEFINED-ON-LINE '127) 
(PUT 'PUTRARRAY 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'PUTRARRAY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DM PUTRARRAY (U)
    (PROG (ARRY INDS VAL)
      (SETQ ARRY (CADR U))
      (SETQ INDS (CDDR U))
      (SETQ VAL (NTH U (LENGTH U)))
      (COND
       ((EQUAL (LENGTH INDS) 2)
        (RETURN (LIST 'PUTV (LIST 'CDR ARRY) (CAR INDS) VAL)))
       (T
        (RETURN
         (LIST 'PUTV (LIST 'CDR ARRY)
               (CONS 'FUNCTIONINDEX2
                     (CONS (LIST 'CAR ARRY)
                           (CONS (CAR INDS) (CONS (CADR INDS) NIL))))
               VAL)))))) 
(PUT 'HILBERTZERODIMP 'NUMBER-OF-ARGS 3) 
(PUT 'HILBERTZERODIMP 'DEFINED-ON-LINE '137) 
(PUT 'HILBERTZERODIMP 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'HILBERTZERODIMP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE HILBERTZERODIMP (NRALL N RARRAY)
    (PROG (I K COUNT VICOUNT)
      (SETQ I 0)
      (SETQ K 0)
      (SETQ COUNT 0)
      (SETQ VICOUNT 0)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (LEQ (SETQ I (PLUS I 1)) NRALL) (LESSP COUNT N)))
          (RETURN NIL)))
        (PROG ()
          (SETQ VICOUNT 1)
          (PROG (K)
            (SETQ K 1)
           LAB
            (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
            (COND
             ((EQUAL (GETV (CDR RARRAY) (FUNCTIONINDEX2 (CAR RARRAY) I K)) 0)
              (SETQ VICOUNT (PLUS VICOUNT 1))))
            (SETQ K (PLUS2 K 1))
            (GO LAB))
          (COND ((EQUAL VICOUNT N) (SETQ COUNT (PLUS COUNT 1)))))
        (GO WHILELABEL))
      (RETURN (EQUAL COUNT N)))) 
(PUT 'GROEZERODIM? 'NUMBER-OF-ARGS 2) 
(PUT 'GROEZERODIM? 'DEFINED-ON-LINE '146) 
(PUT 'GROEZERODIM? 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'GROEZERODIM? 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEZERODIM? (F N)
    (PROG (EXPLIST A R)
      (SETQ R 0)
      (SETQ EXPLIST
              (PROG (FI FORALL-RESULT FORALL-ENDPTR)
                (SETQ FI F)
                (COND ((NULL FI) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (FI) (CADR FI)) (CAR FI)) NIL)))
               LOOPLABEL
                (SETQ FI (CDR FI))
                (COND ((NULL FI) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (FI) (CADR FI)) (CAR FI)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ R (LENGTH F))
      (SETQ A (MAKERARRAY (LIST R N)))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
        (PROG (K)
          (SETQ K 1)
         LAB
          (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
          (PUTV (CDR A) (FUNCTIONINDEX2 (CAR A) I K) (NTH (NTH EXPLIST I) K))
          (SETQ K (PLUS2 K 1))
          (GO LAB))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN (HILBERTZERODIMP R N A)))) 
(PUT 'GZERODIMEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GZERODIMEVAL 'DEFINED-ON-LINE '157) 
(PUT 'GZERODIMEVAL 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'GZERODIMEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GZERODIMEVAL (U)
    (PROG (VL)
      (COND ((CDR U) (SETQ VL (REVAL1 (CADR U) T))))
      (RETURN (GZERODIM1 (REVAL1 (CAR U) T) VL)))) 
(PUT 'GZERODIM? 'PSOPFN 'GZERODIMEVAL) 
(PUT 'GZERODIM1 'NUMBER-OF-ARGS 2) 
(PUT 'GZERODIM1 'DEFINED-ON-LINE '163) 
(PUT 'GZERODIM1 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'GZERODIM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GZERODIM1 (U V)
    (PROG (VARS W OLDORDER)
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GETRLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL W) (RERROR 'GROEBNR2 21 "empty list in hilbertpolynomial")))
      (SETQ VARS (GROEBNERVARS W V))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J) (F2VDP (CAR (SIMP J)))) (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (F2VDP (CAR (SIMP J)))) (CAR J))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (GROEZERODIM? W (LENGTH VARS)))
      (SETKORDER OLDORDER)
      (RETURN (COND (W (NEWHILBI U 'X (CONS 'LIST V))) (T NIL))))) 
(PUT 'GBTEST 'NUMBER-OF-ARGS 1) 
(PUT 'GBTEST 'DEFINED-ON-LINE '175) 
(PUT 'GBTEST 'DEFINED-IN-FILE 'GROEBNER/HILBERTP.RED) 
(PUT 'GBTEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GBTEST (G)
    (PROG (FREDU G1 R S)
      (SETQ G (VDPLSORT G))
      (SETQ G1
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P G)
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (P)
                                    (PROGN
                                     (SETQ R (VDPRED P))
                                     (COND
                                      ((OR (NULL R) (NULL (CADR (CDDR R)))) P)
                                      (T
                                       (VDPSUM (VDPFMON (CADDR P) (CADR P))
                                               (VDPFMON (CADDR R)
                                                        (CADR R)))))))
                                  (CAR P))
                                 NIL)))
               LOOPLABEL
                (SETQ P (CDR P))
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (P)
                            (PROGN
                             (SETQ R (VDPRED P))
                             (COND ((OR (NULL R) (NULL (CADR (CDDR R)))) P)
                                   (T
                                    (VDPSUM (VDPFMON (CADDR P) (CADR P))
                                            (VDPFMON (CADDR R) (CADR R)))))))
                          (CAR P))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT G1) (RETURN NIL)))
        (PROGN
         (PROG (P)
           (SETQ P (CDR G1))
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (COND
               ((NOT (GROEBBUCHCRIT4T (CADR (CAR G1)) (CADR P)))
                (PROGN
                 (SETQ S (GROEBSPOLYNOM (CAR G1) P))
                 (COND
                  ((AND (NOT (OR (NULL S) (NULL (CADR (CDDR S)))))
                        (NULL (GROEBSEARCHINLIST (CADR S) (CDDR G1))))
                   (RERROR 'GROEBNR2 22
                           "****** Not a Groebner basis wrt current ordering")))))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB))
         (COND ((GROEBSEARCHINLIST (CADR (CAR G1)) (CDR G1)) (SETQ FREDU T)))
         (SETQ G1 (CDR G1)))
        (GO WHILELABEL))
      (COND
       (FREDU
        (PROGN
         (TERPRI* T)
         (PRIN2T "WARNING: system is not a fully reduced Groebner basis")
         (PRIN2T "with current term ordering")))))) 
(ENDMODULE) 