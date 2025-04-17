(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFCADPROJ)) 
(REVISION 'OFSFCADPROJ
          "$Id: ofsfcadproj.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFCADPROJ
           "(c) 2000-2009 A. Dolzmann, L. Gilch, A. Seidl, T. Sturm, 2016-2017 T. Sturm") 
(FLUID '(OFSF_CADBVL* *RLPOS)) 
(SWITCH (LIST 'RLCADMC3)) 
(ON1 'RLCADMC3) 
(SWITCH (LIST 'RLPSCSGEN)) 
(ON1 'RLPSCSGEN) 
(PUT 'MTXSF 'ASSERT_DYNTYPECHK 'MTXSFP) 
(FLAG '(MTXSF) 'ASSERT_DYNTYPE) 
(PUT 'SFLIST 'ASSERT_DYNTYPECHK 'SFLISTP) 
(FLAG '(SFLIST) 'ASSERT_DYNTYPE) 
(PUT 'KERNELLIST 'ASSERT_DYNTYPECHK 'KERNELLISTP) 
(FLAG '(KERNELLIST) 'ASSERT_DYNTYPE) 
(PUT 'MTXSFP 'NUMBER-OF-ARGS 1) 
(PUT 'MTXSFP 'DEFINED-ON-LINE '62) 
(PUT 'MTXSFP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCADPROJ.RED) 
(PUT 'MTXSFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MTXSFP (S) (LISTP S)) 
(PUT 'SFLISTP 'NUMBER-OF-ARGS 1) 
(PUT 'SFLISTP 'DEFINED-ON-LINE '65) 
(PUT 'SFLISTP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCADPROJ.RED) 
(PUT 'SFLISTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SFLISTP (S) (OR (NULL S) (AND (PAIRP S) (SFPX (CAR S)) (SFLISTP (CDR S))))) 
(PUT 'KERNELLISTP 'NUMBER-OF-ARGS 1) 
(PUT 'KERNELLISTP 'DEFINED-ON-LINE '68) 
(PUT 'KERNELLISTP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCADPROJ.RED) 
(PUT 'KERNELLISTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KERNELLISTP (S)
    (OR (NULL S)
        (AND (PAIRP S) (ASSERT_KERNELP (CAR S)) (KERNELLISTP (CDR S))))) 
(SWITCH (LIST 'DOLZMANN)) 
(PUT 'OFSF_CADPORDER-BETTERP 'NUMBER-OF-ARGS 4) 
(DE OFSF_CADPORDER-BETTERP (RATING OPTRATING THEO THEOOPT)
    (COND
     ((NOT *DOLZMANN)
      (OR (NOT OPTRATING) (LESSP RATING OPTRATING)
          (AND *RLQEGEN (EQUAL RATING OPTRATING)
               (LESSP (LENGTH THEO) (LENGTH THEOOPT)))))
     (T
      (OR (NOT OPTRATING) (GREATERP RATING OPTRATING)
          (AND *RLQEGEN (EQUAL RATING OPTRATING)
               (LESSP (LENGTH THEO) (LENGTH THEOOPT))))))) 
(PUT 'OFSF_CADPORDER-RATE 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADPORDER-RATE (PSET)
    (PROG (F FORALL-RESULT)
      (SETQ F PSET)
      (SETQ FORALL-RESULT 0)
     LAB1
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (SETQ FORALL-RESULT
              (PLUS ((LAMBDA (F) (SF_STDEG F)) (CAR F)) FORALL-RESULT))
      (SETQ F (CDR F))
      (GO LAB1))) 
(PUT 'OFSF_CADDECDEG 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADDECDEG (PHI)
    (PROG (W)
      (COND (*RLVERBOSE (IOTO_PRIN2 "- decrease degrees: ")))
      (SETQ W (OFSF_DECDEG0 PHI))
      (SETQ PHI (CAR W))
      (COND
       (*RLVERBOSE
        (PROG (X)
          (SETQ X (CDR W))
         LAB
          (COND ((NULL X) (RETURN NIL)))
          ((LAMBDA (X) (IOTO_PRIN2 (LIST "(" (CAR X) "^" (CDR X) ")")))
           (CAR X))
          (SETQ X (CDR X))
          (GO LAB))))
      (RETURN PHI))) 
(PUT 'OFSF_CADVBL 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADVBL (PHI)
    (PROG (VARL QAL CQ CL CLL V Q)
      (PROG (G297)
        (SETQ G297 (OFSF_MKVARL PHI))
        (SETQ VARL (CAR G297))
        (SETQ QAL (CDR G297))
        (RETURN G297))
      (COND
       (QAL
        (PROGN
         (SETQ CQ (CDAR QAL))
         (PROG ()
          WHILELABEL
           (COND ((NOT QAL) (RETURN NIL)))
           (PROGN
            (PROG (G298)
              (SETQ G298 (PROG1 (CAR QAL) (SETQ QAL (CDR QAL))))
              (SETQ V (CAR G298))
              (SETQ Q (CDR G298))
              (RETURN G298))
            (PROG1 (CAR VARL) (SETQ VARL (CDR VARL)))
            (COND
             ((NEQ Q CQ)
              (PROGN
               (PROGN (SETQ CLL (CONS CL CLL)) CL)
               (SETQ CQ Q)
               (SETQ CL NIL))))
            (PROGN (SETQ CL (CONS V CL)) V))
           (GO WHILELABEL))
         (PROG (W1)
           (SETQ W1 (REVERSIP CL))
           (SETQ CLL (CONS W1 CLL))
           (RETURN W1)))))
      (PROGN (SETQ CLL (CONS VARL CLL)) VARL)
      (RETURN (REVERSIP CLL)))) 
(PUT 'OFSF_GCADPORDER 'NUMBER-OF-ARGS 1) 
(DE OFSF_GCADPORDER (PHI)
    (PROG (*RLQEGEN) (SETQ *RLQEGEN T) (RETURN (OFSF_CADPORDER PHI)))) 
(PUT 'OFSF_CADPORDER 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADPORDER (PHI)
    (PROG (CLL *RLCADVERBOSE)
      (COND (*RLVERBOSE (IOTO_PRIN2T "+++ Optimizing projection order.")))
      (COND (*RLCADDECDEG (SETQ PHI (OFSF_CADDECDEG PHI))))
      (SETQ PHI (CL_PNF PHI))
      (SETQ CLL (OFSF_CADVBL PHI))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2 (LIST "+ input order by blocks:"))
         (PROG (CL)
           (SETQ CL CLL)
          LAB
           (COND ((NULL CL) (RETURN NIL)))
           ((LAMBDA (CL) (IOTO_PRIN2 (LIST " -> " CL))) (CAR CL))
           (SETQ CL (CDR CL))
           (GO LAB)))))
      (SETQ CLL (OFSF_CADPORDER1 (OFSF_TRANSFAC (CL_TERML PHI)) CLL))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2 (LIST "+ optimized order:"))
         (PROG (CL)
           (SETQ CL CLL)
          LAB
           (COND ((NULL CL) (RETURN NIL)))
           ((LAMBDA (CL) (IOTO_PRIN2 (LIST " -> " CL))) (CAR CL))
           (SETQ CL (CDR CL))
           (GO LAB)))))
      (RETURN
       (PROG (CL FORALL-RESULT FORALL-ENDPTR)
         (SETQ CL CLL)
        STARTOVER
         (COND ((NULL CL) (RETURN NIL)))
         (SETQ FORALL-RESULT ((LAMBDA (CL) CL) (CAR CL)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ CL (CDR CL))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL CL) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR ((LAMBDA (CL) CL) (CAR CL)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ CL (CDR CL))
         (GO LOOPLABEL))))) 
(PUT 'OFSF_CADPORDER1 'NUMBER-OF-ARGS 2) 
(DE OFSF_CADPORDER1 (FL CLL)
    (PROG (CL LASTP NCL NCLL THEO J)
      (SETQ J 0)
      (SETQ J
              (PROG (CL FORALL-RESULT)
                (SETQ CL CLL)
                (SETQ FORALL-RESULT 0)
               LAB1
                (COND ((NULL CL) (RETURN FORALL-RESULT)))
                (SETQ FORALL-RESULT
                        (PLUS ((LAMBDA (CL) (LENGTH CL)) (CAR CL))
                              FORALL-RESULT))
                (SETQ CL (CDR CL))
                (GO LAB1)))
      (PROG ()
       WHILELABEL
        (COND ((NOT CLL) (RETURN NIL)))
        (PROGN
         (SETQ CL (PROG1 (CAR CLL) (SETQ CLL (CDR CLL))))
         (COND
          (CL
           (PROGN
            (SETQ LASTP (OR (NULL CLL) (NULL (CAR CLL))))
            (PROG (G299 G300)
              (SETQ G299 (OFSF_CADPORDER2 FL CL J LASTP THEO))
              (SETQ G300 G299)
              (SETQ FL (CAR G299))
              (SETQ G299 (CDR G299))
              (SETQ NCL (CAR G299))
              (SETQ G299 (CDR G299))
              (SETQ THEO (CAR G299))
              (SETQ G299 (CDR G299))
              (RETURN G300))
            (PROGN (SETQ NCLL (CONS NCL NCLL)) NCL)
            (SETQ J (DIFFERENCE J (LENGTH CL)))))
          (T (PROGN (SETQ NCLL (CONS NIL NCLL)) NIL))))
        (GO WHILELABEL))
      (RETURN (REVERSIP NCLL)))) 
(PUT 'OFSF_CADPORDER2 'NUMBER-OF-ARGS 5) 
(DE OFSF_CADPORDER2 (FL CL J LASTP THEO)
    (PROG (X NCL)
      (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "+ current input block: " CL))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CL (OR (NOT LASTP) (CDR CL)))) (RETURN NIL)))
        (PROGN
         (PROG (G301 G302)
           (SETQ G301 (OFSF_CADPORDER3 FL CL J THEO))
           (SETQ G302 G301)
           (SETQ FL (CAR G301))
           (SETQ G301 (CDR G301))
           (SETQ X (CAR G301))
           (SETQ G301 (CDR G301))
           (SETQ THEO (CAR G301))
           (SETQ G301 (CDR G301))
           (RETURN G302))
         (PROGN (SETQ NCL (CONS X NCL)) X)
         (SETQ CL (DELETE X CL))
         (SETQ J (DIFFERENCE J 1)))
        (GO WHILELABEL))
      (COND
       (LASTP
        (PROG (W1) (SETQ W1 (CAR CL)) (SETQ NCL (CONS W1 NCL)) (RETURN W1))))
      (SETQ NCL (REVERSIP NCL))
      (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "+ reordered block: " NCL))))
      (RETURN (LIST FL NCL THEO)))) 
(PUT 'OFSF_CADPORDER3 'NUMBER-OF-ARGS 4) 
(DE OFSF_CADPORDER3 (FL CL J THEO)
    (PROG (PP1 PP2 R PSET XOPT PSETOPT ROPT THEOOPT)
      (PROG (X)
        (SETQ X CL)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND (*RLVERBOSE (IOTO_PRIN2 (LIST "[" X ":"))))
            (PROG (G303)
              (SETQ G303 (OFSF_CADPORDER-PROJECT FL X J THEO))
              (SETQ PP1 (CAR G303))
              (SETQ PP2 (CDR G303))
              (RETURN G303))
            (SETQ R (OFSF_CADPORDER-RATE PP1))
            (SETQ PSET (UNION PP1 PP2))
            (COND
             (*RLVERBOSE
              (PROGN
               (IOTO_PRIN2 R)
               (COND (*RLQEGEN (IOTO_PRIN2 (LIST "/" (LENGTH THEO)))))
               (IOTO_PRIN2 "] "))))
            (COND
             ((OFSF_CADPORDER-BETTERP R ROPT THEO THEOOPT)
              (PROGN
               (SETQ XOPT X)
               (SETQ PSETOPT PSET)
               (SETQ ROPT R)
               (COND (*RLQEGEN (SETQ THEOOPT THEO))))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND (*RLVERBOSE (IOTO_PRIN2T (LIST "choose " XOPT))))
      (COND (*RLQEGEN (SETQ THEO THEOOPT)))
      (RETURN (LIST PSETOPT XOPT THEO)))) 
(PUT 'OFSF_CADPORDER-PROJECT 'NUMBER-OF-ARGS 4) 
(DE OFSF_CADPORDER-PROJECT (FL X J THEO)
    (PROG (OLDORDER FFJ FFI PSET W)
      (SETQ OLDORDER (SETKORDER (LIST X)))
      (SETQ FL
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F FL)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (REORDER F)) (CAR F)) NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (REORDER F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG (F)
        (SETQ F FL)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND ((EQ (CAAAR F) X) (PROGN (SETQ FFJ (CONS F FFJ)) F))
                 (T (PROGN (SETQ FFI (CONS F FFI)) F))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND
       (*RLQEGEN
        (PROGN
         (SETQ THEO
                 (PROG (AT FORALL-RESULT FORALL-ENDPTR)
                   (SETQ AT THEO)
                   (COND ((NULL AT) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (AT)
                                       (LIST (CAR AT) (REORDER (CADR AT)) NIL))
                                     (CAR AT))
                                    NIL)))
                  LOOPLABEL
                   (SETQ AT (CDR AT))
                   (COND ((NULL AT) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (AT)
                               (LIST (CAR AT) (REORDER (CADR AT)) NIL))
                             (CAR AT))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ W (OFSF_PROJOPCOHOGEN FFJ X J THEO))
         (SETQ PSET (OFSF_TRANSFAC (CAR W)))))
       (T (SETQ PSET (OFSF_TRANSFAC (OFSF_PROJOPCOHO FFJ X J)))))
      (SETKORDER OLDORDER)
      (RETURN (CONS PSET FFI)))) 
(PUT 'OFSF_CADPROJECTION1 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADPROJECTION1 (CD)
    (PROG (VARL AA FF HH HHTAGS PP THEO W TAG TAGL K R L)
      (SETQ K 0)
      (SETQ R 0)
      (SETQ L 0)
      (SETQ VARL (CADDATA_VARL CD))
      (SETQ K (CADDATA_K CD))
      (SETQ R (LENGTH VARL))
      (SETQ AA (APPEND (CADDATA_AA CD) (CADDATA_AAPLUS CD)))
      (SETQ FF (MKVECT R))
      (SETQ HH (MKVECT R))
      (SETQ HHTAGS (MKVECT R))
      (COND
       (*RLQEGEN
        (PROGN
         (SETQ OFSF_CADBVL* (LTO_DROP VARL K))
         (PROG (G304)
           (SETQ G304 (OFSF_PROJSETCOHOGEN AA VARL NIL))
           (SETQ PP (CAR G304))
           (SETQ THEO (CDR G304))
           (RETURN G304))
         (CADDATA_PUTTHEO CD THEO)))
       (T (SETQ PP (OFSF_PROJSETCOHO AA VARL))))
      (OFSF_DISTRIBUTE PP FF VARL)
      (CADDATA_PUTFF CD FF)
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE R J)) (RETURN NIL)))
        (PROGN
         (SETQ L 0)
         (SETQ TAGL NIL)
         (SETQ W NIL)
         (PROG (F)
           (SETQ F (GETV FF J))
          LAB
           (COND ((NULL F) (RETURN NIL)))
           ((LAMBDA (F)
              (PROGN
               (SETQ L (PLUS L 1))
               (SETQ TAG (OFSF_MKHHTAG J L))
               (PROG (W1)
                 (SETQ W1 (CONS TAG F))
                 (SETQ W (CONS W1 W))
                 (RETURN W1))
               (PROGN (SETQ TAGL (CONS TAG TAGL)) TAG)))
            (CAR F))
           (SETQ F (CDR F))
           (GO LAB))
         (PUTV HH J (REVERSIP W))
         (PUTV HHTAGS J (REVERSIP TAGL)))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (CADDATA_PUTHH CD HH)
      (CADDATA_PUTHHTAGS CD HHTAGS)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T
          (LIST "+ number of all projection factors: " (LENGTH PP)))
         (COND
          (*RLQEGEN
           (IOTO_PRIN2T
            (LIST "+ number of theory elements: " (LENGTH THEO)))))))))) 
(PUT 'OFSF_DISTRIBUTE 'NUMBER-OF-ARGS 3) 
(DE OFSF_DISTRIBUTE (FL FF VARL)
    (PROG (L)
      (SETQ L 0)
      (PROG (F)
        (SETQ F FL)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ L (SF_LEVEL F VARL))
            (COND
             ((AND (GREATERP L 0) (NOT (MEMBER F (GETV FF L))))
              (PUTV FF L (CONS F (GETV FF L)))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB)))) 
(PUT 'OFSF_MKHHTAG 'NUMBER-OF-ARGS 2) 
(DE OFSF_MKHHTAG (J L)
    (INTERN
     (COMPRESS
      (LTO_NCONCN (LIST (EXPLODE 'FF) (EXPLODE J) (EXPLODE '_) (EXPLODE L)))))) 
(PUT 'SF_LEVEL 'NUMBER-OF-ARGS 2) 
(DE SF_LEVEL (F VARL)
    (COND ((NULL VARL) (REDERR "***** sf_level: invalid kernel"))
          ((OR (ATOM F) (ATOM (CAR F))) 0) ((EQ (CAAAR F) (CAR VARL)) 1)
          (T (PLUS 1 (SF_LEVEL F (CDR VARL)))))) 
(PUT 'SF_STDEG 'NUMBER-OF-ARGS 1) 
(DE SF_STDEG (F) (COND ((OR (NULL F) (EQN F 0)) (MINUS 1)) (T (SF_STDEG1 F)))) 
(PUT 'SF_STDEG1 'NUMBER-OF-ARGS 1) 
(DE SF_STDEG1 (F)
    (COND ((OR (ATOM F) (ATOM (CAR F))) 0)
          (T (PLUS (SF_STDEG1 (CDAR F)) (CDAAR F) (SF_STDEG1 (CDR F)))))) 
(PUT 'SF_LC 'NUMBER-OF-ARGS 2) 
(DE SF_LC (F X)
    (COND ((AND (NOT (OR (ATOM F) (ATOM (CAR F)))) (EQ (CAAAR F) X)) (CDAR F))
          (T F))) 
(PUT 'SF_DISCRIMINANT 'NUMBER-OF-ARGS 2) 
(DE SF_DISCRIMINANT (F X)
    ((LAMBDA (*EXP) (QUOTF1 (SFTO_RESF F (CAR (DIFFF F X)) X) (CDAR F))) T)) 
(PUT 'SF_COEFFS 'NUMBER-OF-ARGS 2) 
(DE SF_COEFFS (F X)
    (COND ((OR (OR (ATOM F) (ATOM (CAR F))) (NEQ (CAAAR F) X)) (LIST F))
          (T (COEFFS F)))) 
(PUT 'SF_PSC 'NUMBER-OF-ARGS 4) 
(DE SF_PSC (F G X J) (MTX_DET (MTX_MMJI F G X J J))) 
(PUT 'SF_FACTORS 'NUMBER-OF-ARGS 1) 
(DE SF_FACTORS (F)
    (PROG (W)
      (SETQ W (SFTO_FCTRF F))
      (COND
       ((SFTO_FCTRFPROPERP W)
        (RETURN
         (PROG (PR FORALL-RESULT FORALL-ENDPTR)
           (SETQ PR (CDR W))
           (COND ((NULL PR) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
          LOOPLABEL
           (SETQ PR (CDR PR))
           (COND ((NULL PR) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (RETURN (LIST (SFTO_SQFPARTF F))))) 
(PUT 'SF_PSCS 'NUMBER-OF-ARGS 3) 
(DE SF_PSCS (F G X)
    (PROG (K FORALL-RESULT FORALL-ENDPTR)
      (SETQ K 0)
      (COND
       ((MINUSP
         (DIFFERENCE (DIFFERENCE (MIN (SFTO_VARDEG F X) (SFTO_VARDEG G X)) 1)
                     K))
        (RETURN NIL)))
      (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (SF_PSC F G X K) NIL)))
     LOOPLABEL
      (SETQ K (PLUS2 K 1))
      (COND
       ((MINUSP
         (DIFFERENCE (DIFFERENCE (MIN (SFTO_VARDEG F X) (SFTO_VARDEG G X)) 1)
                     K))
        (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS (SF_PSC F G X K) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'SF_PSCSGEN 'NUMBER-OF-ARGS 4) 
(DE SF_PSCSGEN (A B X THEO)
    (PROG (K PSCL FINISHED)
      (COND ((NOT *RLPSCSGEN) (RETURN (CONS (SF_PSCS A B X) THEO))))
      (SETQ K 0)
      (COND
       ((GREATERP K (DIFFERENCE (MIN (SFTO_VARDEG A X) (SFTO_VARDEG B X)) 1))
        (RETURN (CONS NIL THEO))))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ PSCL (CONS (SF_PSC A B X K) PSCL))
         (SETQ K (PLUS K 1))
         (COND
          ((GREATERP K
                     (DIFFERENCE (MIN (SFTO_VARDEG A X) (SFTO_VARDEG B X)) 1))
           (PROGN (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 "(end)")))))
          ((OFSF_SUREP (LIST 'NEQ (CAR PSCL) NIL) THEO)
           (PROGN
            (COND
             ((OFSF_CADVERBOSEP)
              (COND
               ((OR (ATOM (CAR PSCL)) (ATOM (CAR (CAR PSCL))))
                (IOTO_PRIN2 "(dom)"))
               (T (IOTO_PRIN2 "(=>)")))))
            (SETQ FINISHED T)))
          ((OFSF_CADVALASSP OFSF_CADBVL* (CAR PSCL))
           (PROGN
            (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 "(>th)")))
            (SETQ THEO (CONS (LIST 'NEQ (CAR PSCL) NIL) THEO))
            (SETQ FINISHED T))))
         NIL)
        (COND
         ((NOT
           (OR FINISHED
               (GREATERP K
                         (DIFFERENCE (MIN (SFTO_VARDEG A X) (SFTO_VARDEG B X))
                                     1))))
          (GO REPEATLABEL))))
      (RETURN (CONS PSCL THEO)))) 
(PUT 'SF_DIFF 'NUMBER-OF-ARGS 2) 
(DE SF_DIFF (F X) (CAR (DIFFF F X))) 
(PUT 'MTX_0 'NUMBER-OF-ARGS 2) 
(DE MTX_0 (M N)
    (PROG (L FORALL-RESULT FORALL-ENDPTR)
      (SETQ L 1)
      (COND ((MINUSP (DIFFERENCE M L)) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       (PROG (C FORALL-RESULT FORALL-ENDPTR)
                         (SETQ C 1)
                         (COND ((MINUSP (DIFFERENCE N C)) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR (CONS NIL NIL)))
                        LOOPLABEL
                         (SETQ C (PLUS2 C 1))
                         (COND
                          ((MINUSP (DIFFERENCE N C)) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       NIL)))
     LOOPLABEL
      (SETQ L (PLUS2 L 1))
      (COND ((MINUSP (DIFFERENCE M L)) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               (PROG (C FORALL-RESULT FORALL-ENDPTR)
                 (SETQ C 1)
                 (COND ((MINUSP (DIFFERENCE N C)) (RETURN NIL)))
                 (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS NIL NIL)))
                LOOPLABEL
                 (SETQ C (PLUS2 C 1))
                 (COND ((MINUSP (DIFFERENCE N C)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS NIL NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'MTX_PUT 'NUMBER-OF-ARGS 4) 
(DE MTX_PUT (MTX L C A) (SETCAR (PNTH (NTH MTX L) C) A)) 
(PUT 'MTX_RMLSCS 'NUMBER-OF-ARGS 3) 
(DE MTX_RMLSCS (MTX LINES COLUMNS)
    (PROG (L FORALL-RESULT FORALL-ENDPTR)
      (SETQ L (LTO_RMPOS MTX LINES))
      (COND ((NULL L) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (L) (LTO_RMPOS L COLUMNS)) (CAR L)) NIL)))
     LOOPLABEL
      (SETQ L (CDR L))
      (COND ((NULL L) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (L) (LTO_RMPOS L COLUMNS)) (CAR L)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'MTX_SYLVESTER 'NUMBER-OF-ARGS 3) 
(DE MTX_SYLVESTER (F G X)
    (PROG (RES CFL CGL M N MPN)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ MPN 0)
      (SETQ M (SFTO_VARDEG F X))
      (SETQ N (SFTO_VARDEG G X))
      (SETQ MPN (PLUS M N))
      (COND ((EQN MPN 0) (RETURN (MTX_0 0 0))))
      (SETQ RES (MTX_0 MPN MPN))
      (SETQ CFL (SF_COEFFS F X))
      (SETQ CGL (SF_COEFFS G X))
      (PROG (J)
        (SETQ J 1)
       LAB
        (COND ((MINUSP (DIFFERENCE N J)) (RETURN NIL)))
        (PROG (K)
          (SETQ K J)
         LAB
          (COND ((MINUSP (DIFFERENCE (PLUS J M) K)) (RETURN NIL)))
          (MTX_PUT RES J K (NTH CFL (PLUS 1 (DIFFERENCE K J))))
          (SETQ K (PLUS2 K 1))
          (GO LAB))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (PROG (J)
        (SETQ J (PLUS N 1))
       LAB
        (COND ((MINUSP (DIFFERENCE MPN J)) (RETURN NIL)))
        (PROG (K)
          (SETQ K (DIFFERENCE J N))
         LAB
          (COND ((MINUSP (DIFFERENCE J K)) (RETURN NIL)))
          (MTX_PUT RES J K (NTH CGL (PLUS 1 (DIFFERENCE K (DIFFERENCE J N)))))
          (SETQ K (PLUS2 K 1))
          (GO LAB))
        (SETQ J (PLUS2 J 1))
        (GO LAB))
      (RETURN RES))) 
(PUT 'MTX_DET 'NUMBER-OF-ARGS 1) 
(DE MTX_DET (MTX) (OFSF_DET MTX)) 
(PUT 'MTX_MMJI 'NUMBER-OF-ARGS 5) 
(DE MTX_MMJI (F G X J I)
    (PROG (LTD1 LTD2 CTD1 CTD2 M N MPN)
      (SETQ M 0)
      (SETQ N 0)
      (SETQ MPN 0)
      (SETQ M (SFTO_VARDEG F X))
      (SETQ N (SFTO_VARDEG G X))
      (SETQ MPN (PLUS M N))
      (SETQ LTD1
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K (PLUS (DIFFERENCE MPN J) 1))
                (COND ((MINUSP (DIFFERENCE MPN K)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS K NIL)))
               LOOPLABEL
                (SETQ K (PLUS2 K 1))
                (COND ((MINUSP (DIFFERENCE MPN K)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS K NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ LTD2
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K (PLUS (DIFFERENCE N J) 1))
                (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS K NIL)))
               LOOPLABEL
                (SETQ K (PLUS2 K 1))
                (COND ((MINUSP (DIFFERENCE N K)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS K NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CTD1
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K (PLUS (DIFFERENCE (DIFFERENCE MPN I) J) 1))
                (COND ((MINUSP (DIFFERENCE MPN K)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS K NIL)))
               LOOPLABEL
                (SETQ K (PLUS2 K 1))
                (COND ((MINUSP (DIFFERENCE MPN K)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS K NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CTD2
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K (PLUS (DIFFERENCE MPN (PLUS (TIMES 2 J) 1)) 1))
                (COND
                 ((MINUSP
                   (DIFFERENCE (DIFFERENCE (DIFFERENCE (DIFFERENCE MPN I) J) 1)
                               K))
                  (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS K NIL)))
               LOOPLABEL
                (SETQ K (PLUS2 K 1))
                (COND
                 ((MINUSP
                   (DIFFERENCE (DIFFERENCE (DIFFERENCE (DIFFERENCE MPN I) J) 1)
                               K))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS K NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (MTX_RMLSCS (MTX_SYLVESTER F G X) (UNION LTD1 LTD2) (UNION CTD1 CTD2))))) 
(PUT 'OFSF_PROJSETCOHO 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJSETCOHO (AA VARL)
    (OFSF_PROJSET 'OFSF_TRANSFAC 'OFSF_PROJOPCOHO AA VARL)) 
(PUT 'OFSF_PROJSETCOHOGEN 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJSETCOHOGEN (AA VARL THEO)
    (OFSF_PROJSETGEN 'OFSF_TRANSFAC 'OFSF_PROJOPCOHOGEN AA VARL THEO)) 
(PUT 'OFSF_PROJSET 'NUMBER-OF-ARGS 4) 
(PUT 'OFSF_PROJSET 'DEFINED-ON-LINE '485) 
(PUT 'OFSF_PROJSET 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCADPROJ.RED) 
(PUT 'OFSF_PROJSET 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PROJSET (TRANSFN PROJOPFN AA VARL)
    (OFSF_PROJSET1 TRANSFN PROJOPFN AA VARL 'OFSF_POLYOFLEVEL 'UNION)) 
(PUT 'OFSF_PROJSET1 'NUMBER-OF-ARGS 6) 
(PUT 'OFSF_PROJSET1 'DEFINED-ON-LINE '491) 
(PUT 'OFSF_PROJSET1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCADPROJ.RED) 
(PUT 'OFSF_PROJSET1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PROJSET1 (TRANSFN PROJOPFN AA VARL POLYOFLEVELFN UNIONFN)
    (PROG (R PP CVAR W PPJ)
      (SETQ R (LENGTH VARL))
      (SETQ PP (APPLY TRANSFN (LIST AA)))
      (PROG (J)
        (SETQ J R)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 2 J))) (RETURN NIL)))
        (PROGN
         (SETQ CVAR (NTH VARL J))
         (COND
          ((OFSF_CADVERBOSEP)
           (PROGN
            (IOTO_TPRIN2 (LIST "+ projection F" J " -> F" (DIFFERENCE J 1)))
            (IOTO_TPRIN2T (LIST "+ variable: " CVAR)))))
         (SETQ PPJ (APPLY POLYOFLEVELFN (LIST PP CVAR)))
         (SETQ W (APPLY TRANSFN (LIST (APPLY PROJOPFN (LIST PPJ CVAR J)))))
         (SETQ PP (APPLY UNIONFN (LIST PP W))))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (RETURN PP))) 
(PUT 'OFSF_PROJSETGEN 'NUMBER-OF-ARGS 5) 
(PUT 'OFSF_PROJSETGEN 'DEFINED-ON-LINE '508) 
(PUT 'OFSF_PROJSETGEN 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCADPROJ.RED) 
(PUT 'OFSF_PROJSETGEN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE OFSF_PROJSETGEN (TRANSFN PROJOPFN AA VARL THEO)
    (PROG (R PP CVAR PPJ PP_THEO)
      (SETQ R (LENGTH VARL))
      (SETQ PP (APPLY TRANSFN (LIST AA)))
      (PROG (J)
        (SETQ J R)
       LAB
        (COND ((MINUSP (TIMES (MINUS 1) (DIFFERENCE 2 J))) (RETURN NIL)))
        (PROGN
         (SETQ CVAR (NTH VARL J))
         (COND
          ((OFSF_CADVERBOSEP)
           (PROGN
            (IOTO_TPRIN2 (LIST "+ genprojection F" J " -> F" (DIFFERENCE J 1)))
            (IOTO_PRIN2T (LIST ", variable: " (NTH VARL J))))))
         (SETQ PPJ (OFSF_POLYOFLEVEL PP CVAR))
         (SETQ PP_THEO (APPLY PROJOPFN (LIST PPJ CVAR J THEO)))
         (SETQ PP (UNION (APPLY TRANSFN (LIST (CAR PP_THEO))) PP))
         (SETQ THEO (UNION (CDR PP_THEO) THEO)))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (RETURN (CONS PP THEO)))) 
(PUT 'OFSF_POLYOFLEVEL 'NUMBER-OF-ARGS 2) 
(DE OFSF_POLYOFLEVEL (AA X)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F AA)
     STARTOVER
      (COND ((NULL F) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (F) (COND ((SFTO_MVARTEST F X) (LIST F)))) (CAR F)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ F (CDR F))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (F) (COND ((SFTO_MVARTEST F X) (LIST F)))) (CAR F)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ F (CDR F))
      (GO LOOPLABEL))) 
(PUT 'OFSF_TRANSFAC 'NUMBER-OF-ARGS 1) 
(DE OFSF_TRANSFAC (PP)
    (LTO_LIST2SET
     (PROG (P FORALL-RESULT FORALL-ENDPTR)
       (SETQ P PP)
      STARTOVER
       (COND ((NULL P) (RETURN NIL)))
       (SETQ FORALL-RESULT ((LAMBDA (P) (SF_FACTORS P)) (CAR P)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ P (CDR P))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL P) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR ((LAMBDA (P) (SF_FACTORS P)) (CAR P)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ P (CDR P))
       (GO LOOPLABEL)))) 
(PUT 'OFSF_PROJOPCOHO 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJOPCOHO (AA VAR J)
    (COND ((EQN J 2) (OFSF_PROJCO2V AA VAR))
          ((AND (EQN J 3) *RLCADMC3) (OFSF_PROJMC AA VAR))
          (T (OFSF_PROJCOHO AA VAR)))) 
(PUT 'OFSF_PROJOPCOHOGEN 'NUMBER-OF-ARGS 4) 
(DE OFSF_PROJOPCOHOGEN (AA VAR J THEO)
    (COND ((EQN J 2) (CONS (OFSF_PROJCO2V AA VAR) THEO))
          ((AND (EQN J 3) *RLCADMC3) (OFSF_PROJMCGEN AA VAR THEO))
          (T (OFSF_PROJCOHOGEN AA VAR THEO)))) 
(PUT 'OFSF_PROJCO2V 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJCO2V (AA X)
    (PROG (LL DD RR RESL)
      (SETQ LL (OFSF_PROJLCSLL (LIST AA) X))
      (SETQ DD
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A AA)
               STARTOVER
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (A)
                           (COND
                            ((GEQ (SFTO_VARDEG A X) 2)
                             (LIST (SF_DISCRIMINANT A X)))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A (CDR A))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (A)
                           (COND
                            ((GEQ (SFTO_VARDEG A X) 2)
                             (LIST (SF_DISCRIMINANT A X)))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A (CDR A))
                (GO LOOPLABEL)))
      (SETQ RR
              (PROG (A1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ A1 AA)
               STARTOVER
                (COND ((NULL A1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROG (A2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ A2 (CDR AA))
                          (COND ((NULL A2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (A2)
                                              (SFTO_RESF (CAR A1) A2 X))
                                            (CAR A2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ A2 (CDR A2))
                          (COND ((NULL A2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (A2) (SFTO_RESF (CAR A1) A2 X))
                                    (CAR A2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A1 (CDR A1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROG (A2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ A2 (CDR AA))
                          (COND ((NULL A2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (A2)
                                              (SFTO_RESF (CAR A1) A2 X))
                                            (CAR A2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ A2 (CDR A2))
                          (COND ((NULL A2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (A2) (SFTO_RESF (CAR A1) A2 X))
                                    (CAR A2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A1 (CDR A1))
                (GO LOOPLABEL)))
      (SETQ RESL (LTO_LIST2SET (LTO_REMOVE 'DOMAINP (UNION (UNION LL DD) RR))))
      (COND
       ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(projco2v " (LENGTH RESL) ")"))))
      (RETURN RESL))) 
(PUT 'OFSF_PROJMC 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJMC (AA X)
    (PROG (LL DD RR RESL)
      (SETQ LL (OFSF_PROJMCCOEFFS AA X))
      (SETQ DD
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A AA)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A) (SF_DISCRIMINANT A X)) (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (A) (SF_DISCRIMINANT A X)) (CAR A))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RR
              (PROG (A1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ A1 AA)
               STARTOVER
                (COND ((NULL A1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROG (A2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ A2 (CDR AA))
                          (COND ((NULL A2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (A2)
                                              (SFTO_RESF (CAR A1) A2 X))
                                            (CAR A2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ A2 (CDR A2))
                          (COND ((NULL A2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (A2) (SFTO_RESF (CAR A1) A2 X))
                                    (CAR A2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A1 (CDR A1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROG (A2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ A2 (CDR AA))
                          (COND ((NULL A2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (A2)
                                              (SFTO_RESF (CAR A1) A2 X))
                                            (CAR A2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ A2 (CDR A2))
                          (COND ((NULL A2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (A2) (SFTO_RESF (CAR A1) A2 X))
                                    (CAR A2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A1 (CDR A1))
                (GO LOOPLABEL)))
      (SETQ RESL (LTO_LIST2SET (LTO_REMOVE 'DOMAINP (UNION (UNION LL DD) RR))))
      (COND
       ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(projmc " (LENGTH RESL) ")"))))
      (RETURN RESL))) 
(PUT 'OFSF_PROJMCGEN 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJMCGEN (AA X THEO)
    (PROG (LL DD RR RESL)
      (PROG (G305)
        (SETQ G305 (OFSF_PROJMCCOEFFSGEN AA X THEO))
        (SETQ LL (CAR G305))
        (SETQ THEO (CDR G305))
        (RETURN G305))
      (SETQ DD
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A AA)
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (A) (SF_DISCRIMINANT A X)) (CAR A))
                                 NIL)))
               LOOPLABEL
                (SETQ A (CDR A))
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (A) (SF_DISCRIMINANT A X)) (CAR A))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RR
              (PROG (A1 FORALL-RESULT FORALL-ENDPTR)
                (SETQ A1 AA)
               STARTOVER
                (COND ((NULL A1) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROG (A2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ A2 (CDR AA))
                          (COND ((NULL A2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (A2)
                                              (SFTO_RESF (CAR A1) A2 X))
                                            (CAR A2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ A2 (CDR A2))
                          (COND ((NULL A2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (A2) (SFTO_RESF (CAR A1) A2 X))
                                    (CAR A2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A1 (CDR A1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A1) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROG (A2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ A2 (CDR AA))
                          (COND ((NULL A2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (A2)
                                              (SFTO_RESF (CAR A1) A2 X))
                                            (CAR A2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ A2 (CDR A2))
                          (COND ((NULL A2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (A2) (SFTO_RESF (CAR A1) A2 X))
                                    (CAR A2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A1 (CDR A1))
                (GO LOOPLABEL)))
      (SETQ RESL (LTO_LIST2SET (LTO_REMOVE 'DOMAINP (UNION (UNION LL DD) RR))))
      (COND
       ((OFSF_CADVERBOSEP)
        (IOTO_PRIN2 (LIST "(projmcgen " (LENGTH RESL) ")"))))
      (RETURN (CONS RESL THEO)))) 
(PUT 'OFSF_PROJCOHO 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJCOHO (AA X)
    (PROG (BLL LL SS1 SS2 RESL)
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 "(projcoho ")))
      (SETQ BLL (OFSF_PROJCORED AA X))
      (SETQ LL (LTO_LIST2SET (OFSF_PROJLCSLL BLL X)))
      (SETQ SS1 (LTO_LIST2SET (OFSF_PROJCOSS1 BLL X)))
      (SETQ SS2 (LTO_LIST2SET (OFSF_PROJHOSS2 BLL X)))
      (SETQ RESL (UNION (UNION LL SS1) SS2))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST " " (LENGTH RESL) ")"))))
      (RETURN RESL))) 
(PUT 'OFSF_PROJCOHOGEN 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJCOHOGEN (AA X THEO)
    (PROG (BLL LL SS1 SS2 RESL)
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 "(projcohogen ")))
      (PROG (G306)
        (SETQ G306 (OFSF_PROJCOREDGEN AA X THEO))
        (SETQ BLL (CAR G306))
        (SETQ THEO (CDR G306))
        (RETURN G306))
      (SETQ LL (LTO_LIST2SET (OFSF_PROJLCSLL BLL X)))
      (PROG (G307)
        (SETQ G307 (OFSF_PROJCOSS1GEN BLL X THEO))
        (SETQ SS1 (CAR G307))
        (SETQ THEO (CDR G307))
        (RETURN G307))
      (SETQ SS1 (LTO_LIST2SET SS1))
      (PROG (G308)
        (SETQ G308 (OFSF_PROJHOSS2GEN BLL X THEO))
        (SETQ SS2 (CAR G308))
        (SETQ THEO (CDR G308))
        (RETURN G308))
      (SETQ SS2 (LTO_LIST2SET SS2))
      (SETQ RESL (UNION (UNION LL SS1) SS2))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST " " (LENGTH RESL) ")"))))
      (RETURN (CONS RESL THEO)))) 
(PUT 'OFSF_PROJCORED 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJCORED (AA X)
    (PROG (W RESLL C)
      (SETQ C 0)
      (PROG (F)
        (SETQ F AA)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ W (OFSF_PROJCORED1 F X))
            (SETQ W (OFSF_DEFPDEL W NIL))
            (COND (W (PROGN (SETQ RESLL (CONS W RESLL)) W)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ RESLL (REVERSIP RESLL))
      (COND
       ((OFSF_CADVERBOSEP)
        (PROGN
         (SETQ C
                 (PROG (L FORALL-RESULT)
                   (SETQ L RESLL)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND ((NULL L) (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (PLUS ((LAMBDA (L) (LENGTH L)) (CAR L))
                                 FORALL-RESULT))
                   (SETQ L (CDR L))
                   (GO LAB1)))
         (IOTO_PRIN2 (LIST "(red " C ")")))))
      (RETURN RESLL))) 
(PUT 'OFSF_PROJCORED1 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJCORED1 (F X)
    (PROG (LCF FINISHED RESL LTHEO)
      (COND (NIL NIL))
      (PROG ()
       REPEATLABEL
        (PROGN
         (COND
          ((SFTO_MVARTEST F X)
           (PROGN
            (SETQ LCF (CDAR F))
            (COND
             ((OFSF_SUREP (LIST 'NEQ LCF NIL) LTHEO)
              (PROGN (SETQ FINISHED T) (PROGN (SETQ RESL (CONS F RESL)) F)))
             ((NOT (OFSF_SUREP (LIST 'EQUAL LCF NIL) LTHEO))
              (PROGN
               (PROGN (SETQ RESL (CONS F RESL)) F)
               (PROG (W1)
                 (SETQ W1 (LIST 'EQUAL LCF NIL))
                 (SETQ LTHEO (CONS W1 LTHEO))
                 (RETURN W1)))))
            (SETQ F (CDR F))))
          (T
           (PROGN
            (SETQ FINISHED T)
            (COND
             ((NOT (OFSF_SUREP (LIST 'EQUAL F NIL) LTHEO))
              (PROGN (SETQ RESL (CONS F RESL)) F)))))))
        (COND ((NOT FINISHED) (GO REPEATLABEL))))
      (RETURN (REVERSIP RESL)))) 
(PUT 'OFSF_PROJCOREDGEN 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJCOREDGEN (AA X THEO)
    (PROG (W RESLL C)
      (SETQ C 0)
      (PROG (F)
        (SETQ F AA)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (PROG (G309)
              (SETQ G309 (OFSF_PROJCOREDGEN1 F X THEO))
              (SETQ W (CAR G309))
              (SETQ THEO (CDR G309))
              (RETURN G309))
            (SETQ W (OFSF_DEFPDEL W THEO))
            (COND (W (PROGN (SETQ RESLL (CONS W RESLL)) W)))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ RESLL (REVERSIP RESLL))
      (COND
       ((OFSF_CADVERBOSEP)
        (PROGN
         (SETQ C
                 (PROG (L FORALL-RESULT)
                   (SETQ L RESLL)
                   (SETQ FORALL-RESULT 0)
                  LAB1
                   (COND ((NULL L) (RETURN FORALL-RESULT)))
                   (SETQ FORALL-RESULT
                           (PLUS ((LAMBDA (L) (LENGTH L)) (CAR L))
                                 FORALL-RESULT))
                   (SETQ L (CDR L))
                   (GO LAB1)))
         (IOTO_PRIN2 (LIST "(redgen " C ")")))))
      (RETURN (CONS RESLL THEO)))) 
(PUT 'OFSF_PROJCOREDGEN1 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJCOREDGEN1 (F X THEO)
    (PROG (CTHEO LTHEO LCF FINISHED RESL)
      (COND (NIL NIL))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ CTHEO (APPEND THEO LTHEO))
         (COND
          ((SFTO_MVARTEST F X)
           (PROGN
            (SETQ LCF (CDAR F))
            (COND
             ((OFSF_SUREP (LIST 'NEQ LCF NIL) CTHEO)
              (PROGN (SETQ FINISHED T) (PROGN (SETQ RESL (CONS F RESL)) F)))
             ((NOT (OFSF_SUREP (LIST 'EQUAL LCF NIL) CTHEO))
              (PROGN
               (COND
                ((OFSF_CADVALASSP OFSF_CADBVL* LCF)
                 (PROGN
                  (SETQ FINISHED T)
                  (PROG (W1)
                    (SETQ W1 (LIST 'NEQ LCF NIL))
                    (SETQ THEO (CONS W1 THEO))
                    (RETURN W1))))
                (T
                 (PROG (W1)
                   (SETQ W1 (LIST 'EQUAL LCF NIL))
                   (SETQ LTHEO (CONS W1 LTHEO))
                   (RETURN W1))))
               (PROGN (SETQ RESL (CONS F RESL)) F))))
            (SETQ F (CDR F))))
          (T
           (PROGN
            (SETQ FINISHED T)
            (COND
             ((NOT (OFSF_SUREP (LIST 'EQUAL F NIL) CTHEO))
              (PROGN (SETQ RESL (CONS F RESL)) F)))))))
        (COND ((NOT FINISHED) (GO REPEATLABEL))))
      (RETURN (CONS (REVERSIP RESL) THEO)))) 
(PUT 'OFSF_PROJCOSS1 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJCOSS1 (BLL X)
    (PROG (W RESL)
      (PROG (BL)
        (SETQ BL BLL)
       LAB
        (COND ((NULL BL) (RETURN NIL)))
        ((LAMBDA (BL)
           (PROGN
            (SETQ W
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F BL)
                     STARTOVER
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (F) (SF_PSCS F (SF_DIFF F X) X))
                               (CAR F)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ F (CDR F))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (F) (SF_PSCS F (SF_DIFF F X) X))
                               (CAR F)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ F (CDR F))
                      (GO LOOPLABEL)))
            (SETQ W (OFSF_DEFPDEL W NIL))
            (SETQ RESL (APPEND RESL W))))
         (CAR BL))
        (SETQ BL (CDR BL))
        (GO LAB))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(S1 " (LENGTH RESL) ")"))))
      (RETURN RESL))) 
(PUT 'OFSF_PROJCOSS1GEN 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJCOSS1GEN (BLL X THEO)
    (PROG (W PSCS RESL)
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 "(S1gen ")))
      (PROG (BL)
        (SETQ BL BLL)
       LAB
        (COND ((NULL BL) (RETURN NIL)))
        ((LAMBDA (BL)
           (PROGN
            (SETQ W
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F BL)
                     STARTOVER
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (F)
                                 (PROGN
                                  (PROG (G310)
                                    (SETQ G310
                                            (SF_PSCSGEN F (SF_DIFF F X) X
                                             THEO))
                                    (SETQ PSCS (CAR G310))
                                    (SETQ THEO (CDR G310))
                                    (RETURN G310))
                                  PSCS))
                               (CAR F)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ F (CDR F))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (F)
                                 (PROGN
                                  (PROG (G310)
                                    (SETQ G310
                                            (SF_PSCSGEN F (SF_DIFF F X) X
                                             THEO))
                                    (SETQ PSCS (CAR G310))
                                    (SETQ THEO (CDR G310))
                                    (RETURN G310))
                                  PSCS))
                               (CAR F)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ F (CDR F))
                      (GO LOOPLABEL)))
            (SETQ W (OFSF_DEFPDEL W THEO))
            (SETQ RESL (APPEND RESL W))))
         (CAR BL))
        (SETQ BL (CDR BL))
        (GO LAB))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST (LENGTH RESL) ")"))))
      (RETURN (CONS RESL THEO)))) 
(PUT 'OFSF_PROJHOSS2 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJHOSS2 (BLL X)
    (PROG (BL W RESL REDLL)
      (PROG ()
       WHILELABEL
        (COND ((NOT BLL) (RETURN NIL)))
        (PROGN
         (SETQ BL (PROG1 (CAR BLL) (SETQ BLL (CDR BLL))))
         (SETQ W
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F BL)
                  STARTOVER
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (F)
                              (PROG (TMP FORALL-RESULT FORALL-ENDPTR)
                                (SETQ TMP BLL)
                               STARTOVER
                                (COND ((NULL TMP) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        ((LAMBDA (TMP) (SF_PSCS F (CAR TMP) X))
                                         (CAR TMP)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                (SETQ TMP (CDR TMP))
                                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                               LOOPLABEL
                                (COND ((NULL TMP) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        ((LAMBDA (TMP) (SF_PSCS F (CAR TMP) X))
                                         (CAR TMP)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                (SETQ TMP (CDR TMP))
                                (GO LOOPLABEL)))
                            (CAR F)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ F (CDR F))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (F)
                              (PROG (TMP FORALL-RESULT FORALL-ENDPTR)
                                (SETQ TMP BLL)
                               STARTOVER
                                (COND ((NULL TMP) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        ((LAMBDA (TMP) (SF_PSCS F (CAR TMP) X))
                                         (CAR TMP)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                (SETQ TMP (CDR TMP))
                                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                               LOOPLABEL
                                (COND ((NULL TMP) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        ((LAMBDA (TMP) (SF_PSCS F (CAR TMP) X))
                                         (CAR TMP)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                (SETQ TMP (CDR TMP))
                                (GO LOOPLABEL)))
                            (CAR F)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ F (CDR F))
                   (GO LOOPLABEL)))
         (SETQ W (OFSF_DEFPDEL W NIL))
         (SETQ RESL (APPEND RESL W)))
        (GO WHILELABEL))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(S2 " (LENGTH RESL) ")"))))
      (RETURN RESL))) 
(PUT 'OFSF_PROJHOSS2GEN 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJHOSS2GEN (BLL X THEO)
    (PROG (BL W PSCS RESL)
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 "(S2gen ")))
      (PROG ()
       WHILELABEL
        (COND ((NOT BLL) (RETURN NIL)))
        (PROGN
         (SETQ BL (PROG1 (CAR BLL) (SETQ BLL (CDR BLL))))
         (SETQ W
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F BL)
                  STARTOVER
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (F)
                              (PROG (TMP FORALL-RESULT FORALL-ENDPTR)
                                (SETQ TMP BLL)
                               STARTOVER
                                (COND ((NULL TMP) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        ((LAMBDA (TMP)
                                           (PROGN
                                            (PROG (G311)
                                              (SETQ G311
                                                      (SF_PSCSGEN F (CAR TMP) X
                                                       THEO))
                                              (SETQ PSCS (CAR G311))
                                              (SETQ THEO (CDR G311))
                                              (RETURN G311))
                                            PSCS))
                                         (CAR TMP)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                (SETQ TMP (CDR TMP))
                                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                               LOOPLABEL
                                (COND ((NULL TMP) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        ((LAMBDA (TMP)
                                           (PROGN
                                            (PROG (G311)
                                              (SETQ G311
                                                      (SF_PSCSGEN F (CAR TMP) X
                                                       THEO))
                                              (SETQ PSCS (CAR G311))
                                              (SETQ THEO (CDR G311))
                                              (RETURN G311))
                                            PSCS))
                                         (CAR TMP)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                (SETQ TMP (CDR TMP))
                                (GO LOOPLABEL)))
                            (CAR F)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ F (CDR F))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (F)
                              (PROG (TMP FORALL-RESULT FORALL-ENDPTR)
                                (SETQ TMP BLL)
                               STARTOVER
                                (COND ((NULL TMP) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        ((LAMBDA (TMP)
                                           (PROGN
                                            (PROG (G311)
                                              (SETQ G311
                                                      (SF_PSCSGEN F (CAR TMP) X
                                                       THEO))
                                              (SETQ PSCS (CAR G311))
                                              (SETQ THEO (CDR G311))
                                              (RETURN G311))
                                            PSCS))
                                         (CAR TMP)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                                (SETQ TMP (CDR TMP))
                                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                               LOOPLABEL
                                (COND ((NULL TMP) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        ((LAMBDA (TMP)
                                           (PROGN
                                            (PROG (G311)
                                              (SETQ G311
                                                      (SF_PSCSGEN F (CAR TMP) X
                                                       THEO))
                                              (SETQ PSCS (CAR G311))
                                              (SETQ THEO (CDR G311))
                                              (RETURN G311))
                                            PSCS))
                                         (CAR TMP)))
                                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                                (SETQ TMP (CDR TMP))
                                (GO LOOPLABEL)))
                            (CAR F)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ F (CDR F))
                   (GO LOOPLABEL)))
         (SETQ W (OFSF_DEFPDEL W THEO))
         (SETQ RESL (APPEND RESL W)))
        (GO WHILELABEL))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST (LENGTH RESL) ")"))))
      (RETURN (CONS RESL THEO)))) 
(PUT 'OFSF_PROJLCSLL 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJLCSLL (BLL X)
    (PROG (W RESL)
      (PROG (BL)
        (SETQ BL BLL)
       LAB
        (COND ((NULL BL) (RETURN NIL)))
        ((LAMBDA (BL)
           (PROGN
            (SETQ W
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F BL)
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (F) (SF_LC F X)) (CAR F))
                                            NIL)))
                     LOOPLABEL
                      (SETQ F (CDR F))
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (F) (SF_LC F X)) (CAR F)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (SETQ W (OFSF_DEFPDEL W NIL))
            (SETQ RESL (APPEND RESL W))))
         (CAR BL))
        (SETQ BL (CDR BL))
        (GO LAB))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(lcs " (LENGTH RESL) ")"))))
      (RETURN RESL))) 
(PUT 'OFSF_PROJMCCOEFFS 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJMCCOEFFS (AA X)
    (PROG (RESL)
      (SETQ RESL
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F AA)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F) (OFSF_PROJMCCOEFFS1 F X)) (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F) (OFSF_PROJMCCOEFFS1 F X)) (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (SETQ RESL (OFSF_DEFPDEL RESL NIL))
      (COND
       ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(coeffs " (LENGTH RESL) ")"))))
      (RETURN RESL))) 
(PUT 'OFSF_PROJMCCOEFFS1 'NUMBER-OF-ARGS 2) 
(DE OFSF_PROJMCCOEFFS1 (F X)
    (PROG (LCF FINISHED RESL LTHEO)
      (COND (NIL NIL))
      (PROG ()
       REPEATLABEL
        (PROGN
         (COND
          ((SFTO_MVARTEST F X)
           (PROGN
            (SETQ LCF (CDAR F))
            (COND ((OFSF_SUREP (LIST 'NEQ LCF NIL) LTHEO) (SETQ FINISHED T))
                  ((NOT (OFSF_SUREP (LIST 'EQUAL LCF NIL) LTHEO))
                   (PROGN
                    (PROGN (SETQ RESL (CONS LCF RESL)) LCF)
                    (PROG (W1)
                      (SETQ W1 (LIST 'EQUAL LCF NIL))
                      (SETQ LTHEO (CONS W1 LTHEO))
                      (RETURN W1)))))
            (SETQ F (CDR F))
            NIL))
          (T
           (PROGN
            (SETQ FINISHED T)
            (COND
             ((NOT (OFSF_SUREP (LIST 'EQUAL F NIL) LTHEO))
              (PROGN (SETQ RESL (CONS F RESL)) F)))))))
        (COND ((NOT FINISHED) (GO REPEATLABEL))))
      (RETURN (REVERSIP RESL)))) 
(PUT 'OFSF_PROJMCCOEFFSGEN 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJMCCOEFFSGEN (AA X THEO)
    (PROG (RESL W)
      (SETQ RESL
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F AA)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (PROGN
                            (PROG (G312)
                              (SETQ G312 (OFSF_PROJMCCOEFFSGEN1 F X THEO))
                              (SETQ W (CAR G312))
                              (SETQ THEO (CDR G312))
                              (RETURN G312))
                            W))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (PROGN
                            (PROG (G312)
                              (SETQ G312 (OFSF_PROJMCCOEFFSGEN1 F X THEO))
                              (SETQ W (CAR G312))
                              (SETQ THEO (CDR G312))
                              (RETURN G312))
                            W))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (SETQ RESL (OFSF_DEFPDEL RESL THEO))
      (COND
       ((OFSF_CADVERBOSEP)
        (IOTO_PRIN2 (LIST "(coeffsgen " (LENGTH RESL) ")"))))
      (RETURN (CONS RESL THEO)))) 
(PUT 'OFSF_PROJMCCOEFFSGEN1 'NUMBER-OF-ARGS 3) 
(DE OFSF_PROJMCCOEFFSGEN1 (F X THEO)
    (PROG (CTHEO LTHEO LCF FINISHED RESL)
      (COND (NIL NIL))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ CTHEO (APPEND THEO LTHEO))
         (COND
          ((SFTO_MVARTEST F X)
           (PROGN
            (SETQ LCF (CDAR F))
            (COND ((OFSF_SUREP (LIST 'NEQ LCF NIL) CTHEO) (SETQ FINISHED T))
                  ((NOT (OFSF_SUREP (LIST 'EQUAL LCF NIL) CTHEO))
                   (COND
                    ((OFSF_CADVALASSP OFSF_CADBVL* LCF)
                     (PROGN
                      (SETQ FINISHED T)
                      (PROG (W1)
                        (SETQ W1 (LIST 'NEQ LCF NIL))
                        (SETQ THEO (CONS W1 THEO))
                        (RETURN W1))))
                    (T
                     (PROGN
                      (PROGN (SETQ RESL (CONS LCF RESL)) LCF)
                      (PROG (W1)
                        (SETQ W1 (LIST 'EQUAL LCF NIL))
                        (SETQ LTHEO (CONS W1 LTHEO))
                        (RETURN W1)))))))
            (SETQ F (CDR F))))
          (T
           (PROGN
            (SETQ FINISHED T)
            (COND
             ((NOT (OFSF_SUREP (LIST 'EQUAL F NIL) LTHEO))
              (PROGN (SETQ RESL (CONS F RESL)) F)))))))
        (COND ((NOT FINISHED) (GO REPEATLABEL))))
      (RETURN (CONS (REVERSIP RESL) THEO)))) 
(PUT 'OFSF_DEFPDEL 'NUMBER-OF-ARGS 2) 
(DE OFSF_DEFPDEL (L THEO)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F L)
     STARTOVER
      (COND ((NULL F) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (F)
                 (COND ((NOT (OFSF_SUREP (LIST 'NEQ F NIL) THEO)) (LIST F))
                       (T
                        (PROGN
                         (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 "*")))
                         NIL))))
               (CAR F)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ F (CDR F))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (F)
                 (COND ((NOT (OFSF_SUREP (LIST 'NEQ F NIL) THEO)) (LIST F))
                       (T
                        (PROGN
                         (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 "*")))
                         NIL))))
               (CAR F)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ F (CDR F))
      (GO LOOPLABEL))) 
(PUT 'OFSF_CADVALASSP 'NUMBER-OF-ARGS 2) 
(DE OFSF_CADVALASSP (BVL F)
    (AND (OR *RLQEGENCT (SFTO_MONFP F)) (NULL (INTERSECTION BVL (KERNELS F))))) 
(ENDMODULE) 