(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SFTORESULTANT)) 
(REVISION 'SFTORESULTANT
          "$Id: sftoresultant.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'SFTORESULTANT "(c) 2013-2017 M. Kosta, T. Sturm") 
(SWITCH (LIST 'FASTRESEXPAND)) 
(ON1 'FASTRESEXPAND) 
(SWITCH (LIST 'FASTRESVB)) 
(FLAG '(FASTRESULTANT) 'OPFN) 
(PUT 'FASTRESULTANT 'NUMBER-OF-ARGS 3) 
(DE FASTRESULTANT (F G X)
    (PROG (W RES)
      (COND ((NOT (IDP X)) (REDERR "third argument must be a variable")))
      (SETQ W (SFTO_RESF (CAR (SIMP F)) (CAR (SIMP G)) X))
      (COND
       ((NOT *FASTRESEXPAND)
        (RETURN
         (CONS 'LIST
               (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                 (SETQ PR W)
                 (COND ((NULL PR) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (PR)
                                     (LIST 'LIST (MK*SQ (CONS (CAR PR) 1))
                                           (CADR PR)))
                                   (CAR PR))
                                  NIL)))
                LOOPLABEL
                 (SETQ PR (CDR PR))
                 (COND ((NULL PR) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (PR)
                             (LIST 'LIST (MK*SQ (CONS (CAR PR) 1)) (CADR PR)))
                           (CAR PR))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN (MK*SQ (CONS W 1))))) 
(PUT 'SFTO_RESF 'NUMBER-OF-ARGS 3) 
(DE SFTO_RESF (F G X)
    (PROG (OO W RES)
      (SETQ OO (SETKORDER (CONS X KORD*)))
      (SETQ F (REORDER F))
      (SETQ G (REORDER G))
      (SETQ W
              (ERRORSET (LIST 'SFTO_RESF2 (MKQUOTE F) (MKQUOTE G) (MKQUOTE X))
                        NIL *BACKTRACE))
      (SETKORDER OO)
      (COND ((ERRORP W) (REDERR EMSG*)))
      (COND
       ((NOT *FASTRESEXPAND)
        (RETURN
         (PROG (PR FORALL-RESULT FORALL-ENDPTR)
           (SETQ PR (CAR W))
           (COND ((NULL PR) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (PR) (LIST (REORDER (CAR PR)) (CADR PR)))
                             (CAR PR))
                            NIL)))
          LOOPLABEL
           (SETQ PR (CDR PR))
           (COND ((NULL PR) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (PR) (LIST (REORDER (CAR PR)) (CADR PR)))
                     (CAR PR))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))))
      (SETQ RES 1)
      (PROG (PR)
        (SETQ PR (CAR W))
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (SETQ RES
                   ((LAMBDA (G177)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF RES G177))
                            (T (POLY-MULTF RES G177))))
                    (EXPTF (REORDER (CAR PR)) (CADR PR)))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (RETURN RES))) 
(PUT 'SFTO_RESF2 'NUMBER-OF-ARGS 3) 
(DE SFTO_RESF2 (F G X)
    (PROG (W)
      (COND
       ((OR (NULL F) (NULL G) (ZEROP F) (ZEROP G)) (RETURN (LIST (LIST 0 1)))))
      (COND
       ((OR (OR (ATOM F) (ATOM (CAR F))) (NULL (EQ (CAAAR F) X)))
        (RETURN
         (COND
          ((AND (NOT (OR (ATOM G) (ATOM (CAR G)))) (EQ (CAAAR G) X))
           (LIST (LIST F (CDAAR G))))
          (T (LIST (LIST 1 1)))))))
      (COND
       ((OR (OR (ATOM G) (ATOM (CAR G))) (NULL (EQ (CAAAR G) X)))
        (RETURN
         (COND ((EQ (CAAAR F) X) (LIST (LIST G (CDAAR F))))
               (T (LIST (LIST 1 1)))))))
      (SETQ W (SFTO_GCDF F G))
      (COND ((NOT (OR (ATOM W) (ATOM (CAR W)))) (RETURN (LIST (LIST 0 1)))))
      (RETURN
       (CONS (LIST W (PLUS (CDAAR F) (CDAAR G)))
             (SFTO_RESF3 (QUOTFX F W) (QUOTFX G W) X))))) 
(PUT 'SFTO_RESF3 'NUMBER-OF-ARGS 3) 
(DE SFTO_RESF3 (F G X)
    (PROG (CNT VARL DEGAL L)
      (SETQ L 0)
      (SETQ CNT (SFTO_UCONTENTF F))
      (COND
       ((NEQ CNT 1)
        (RETURN (CONS (LIST CNT (CDAAR G)) (SFTO_RESF2 (QUOTFX F CNT) G X)))))
      (SETQ CNT (SFTO_UCONTENTF G))
      (COND
       ((NEQ CNT 1)
        (RETURN (CONS (LIST CNT (CDAAR F)) (SFTO_RESF2 F (QUOTFX G CNT) X)))))
      (SETQ L (SFTO_MINDEG1 F X))
      (COND
       ((GREATERP L 0)
        (RETURN
         (CONS (LIST (SFTO_TC G X) L)
               (SFTO_RESF2 (QUOTFX F (EXPTF (LIST (CONS (CONS X 1) 1)) L)) G
                X)))))
      (SETQ L (SFTO_MINDEG1 G X))
      (COND
       ((GREATERP L 0)
        (RETURN
         (CONS (LIST (SFTO_TC F X) L)
               (SFTO_RESF2 F (QUOTFX G (EXPTF (LIST (CONS (CONS X 1) 1)) L))
                X)))))
      (SETQ DEGAL (SFTO_DEGGCD F G (LIST X)))
      (COND
       ((NOT (NULL DEGAL))
        (RETURN
         (SFTO_RAISEL
          (SFTO_RESF2 (SFTO_SUBFWD F DEGAL) (SFTO_SUBFWD G DEGAL) X)
          (CDR (ATSOC X DEGAL))))))
      (SETQ VARL (SETDIFF (UNION (KERNELS F) (KERNELS G)) (LIST X)))
      (SETQ DEGAL (SFTO_DEGGCD F G VARL))
      (COND
       ((NOT (NULL DEGAL))
        (PROGN
         (RETURN
          (SFTO_SUBBCKL
           (SFTO_BEZOUT_RESULTANT_FAC (SFTO_SUBFWD F DEGAL)
            (SFTO_SUBFWD G DEGAL) X)
           DEGAL))
         NIL)))
      (RETURN (SFTO_BEZOUT_RESULTANT_FAC F G X)))) 
(PUT 'SFTO_MINDEG 'NUMBER-OF-ARGS 2) 
(DE SFTO_MINDEG (F X) NIL) 
(PUT 'SFTO_MINDEG1 'NUMBER-OF-ARGS 2) 
(DE SFTO_MINDEG1 (F X)
    (COND ((OR (OR (ATOM F) (ATOM (CAR F))) (NEQ (CAAAR F) X)) 0)
          ((CDR F) (SFTO_MINDEG1 (CDR F) X)) (T (CDAAR F)))) 
(PUT 'SFTO_TC 'NUMBER-OF-ARGS 2) 
(DE SFTO_TC (F X)
    (COND ((OR (ATOM F) (ATOM (CAR F))) F)
          ((EQ (CAAAR F) X)
           (COND ((EQ (CDR F) NIL) (CDAR F)) (T (SFTO_TC (CDR F) X))))
          (T F))) 
(PUT 'SFTO_DEGGCD 'NUMBER-OF-ARGS 3) 
(DE SFTO_DEGGCD (F G VARL)
    (PROG (VAR RES C)
      (SETQ C 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT VARL) (RETURN NIL)))
        (PROGN
         (SETQ VAR (PROG1 (CAR VARL) (SETQ VARL (CDR VARL))))
         (SETQ C (SFTO_DEGGCD1 F VAR))
         (COND
          ((NEQ C 1)
           (PROGN
            (SETQ C (GCDN C (SFTO_DEGGCD1 G VAR)))
            (COND ((NEQ C 1) (SETQ RES (CONS (CONS VAR C) RES))))))))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'SFTO_DEGGCD1 'NUMBER-OF-ARGS 2) 
(DE SFTO_DEGGCD1 (F X)
    (PROG (C RES)
      (SETQ RES 0)
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C (NEQ RES 1))) (RETURN NIL)))
        (COND ((OR (ATOM F) (ATOM (CAR F))) (SETQ C NIL))
              (T
               (PROGN
                (COND ((EQ (CAAAR F) X) (SETQ RES (GCDN RES (CDAAR F))))
                      (T (SETQ RES (GCDN RES (SFTO_DEGGCD1 (CDAR F) X)))))
                (SETQ F (CDR F)))))
        (GO WHILELABEL))
      (RETURN RES))) 
(PUT 'SFTO_RAISEL 'NUMBER-OF-ARGS 2) 
(DE SFTO_RAISEL (POLYL C)
    (PROG (PR FORALL-RESULT FORALL-ENDPTR)
      (SETQ PR POLYL)
      (COND ((NULL PR) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (PR) (LIST (CAR PR) (TIMES (CADR PR) C)))
                        (CAR PR))
                       NIL)))
     LOOPLABEL
      (SETQ PR (CDR PR))
      (COND ((NULL PR) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (PR) (LIST (CAR PR) (TIMES (CADR PR) C))) (CAR PR))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'SFTO_SUBFWD 'NUMBER-OF-ARGS 2) 
(DE SFTO_SUBFWD (POLY DEGAL)
    (PROG (W D)
      (SETQ D 0)
      (COND ((OR (ATOM POLY) (ATOM (CAR POLY))) (RETURN POLY)))
      (SETQ W (ATSOC (CAAAR POLY) DEGAL))
      (SETQ D (COND (W (QUOTIENT (CDAAR POLY) (CDR W))) (T (CDAAR POLY))))
      (RETURN
       (CONS (CONS (CONS (CAAAR POLY) D) (SFTO_SUBFWD (CDAR POLY) DEGAL))
             (SFTO_SUBFWD (CDR POLY) DEGAL))))) 
(PUT 'SFTO_SUBBCKL 'NUMBER-OF-ARGS 2) 
(DE SFTO_SUBBCKL (POLYL DEGAL)
    (PROG (PR FORALL-RESULT FORALL-ENDPTR)
      (SETQ PR POLYL)
      (COND ((NULL PR) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (PR)
                          (LIST (SFTO_SUBBCK (CAR PR) DEGAL) (CADR PR)))
                        (CAR PR))
                       NIL)))
     LOOPLABEL
      (SETQ PR (CDR PR))
      (COND ((NULL PR) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (PR) (LIST (SFTO_SUBBCK (CAR PR) DEGAL) (CADR PR)))
                (CAR PR))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'SFTO_SUBBCK 'NUMBER-OF-ARGS 2) 
(DE SFTO_SUBBCK (POLY DEGAL)
    (PROG (W D)
      (SETQ D 0)
      (COND ((OR (ATOM POLY) (ATOM (CAR POLY))) (RETURN POLY)))
      (SETQ W (ATSOC (CAAAR POLY) DEGAL))
      (SETQ D (COND (W (TIMES (CDAAR POLY) (CDR W))) (T (CDAAR POLY))))
      (RETURN
       (CONS (CONS (CONS (CAAAR POLY) D) (SFTO_SUBBCK (CDAR POLY) DEGAL))
             (SFTO_SUBBCK (CDR POLY) DEGAL))))) 
(PUT 'SFTO_BEZOUT_RESULTANT_FAC 'NUMBER-OF-ARGS 3) 
(DE SFTO_BEZOUT_RESULTANT_FAC (U V W)
    (PROG (N NM AP EP UH UT VH VT X CX CXF CEP CUH CVH)
      (SETQ N 0)
      (SETQ NM 0)
      (COND
       (*FASTRESVB (PROGN (PROGN (PRIN2 "entering bezout.") NIL) (TERPRI))))
      (SETQ *EXP T)
      (COND
       ((OR (OR (ATOM U) (ATOM (CAR U))) (NULL (EQ (CAAAR U) W)))
        (RETURN
         (COND
          ((AND (NOT (OR (ATOM V) (ATOM (CAR V)))) (EQ (CAAAR V) W))
           (LIST (LIST U (CDAAR V))))
          (T (LIST (LIST 1 1))))))
       ((OR (OR (ATOM V) (ATOM (CAR V))) (NULL (EQ (CAAAR V) W)))
        (RETURN
         (COND ((EQ (CAAAR U) W) (LIST (LIST V (CDAAR U))))
               (T (LIST (LIST 1 1)))))))
      (SETQ N (DIFFERENCE (CDAAR V) (CDAAR U)))
      (SETQ CXF '(1))
      (COND
       ((LESSP N 0)
        (PROGN
         (SETQ X U)
         (SETQ U V)
         (SETQ V X)
         (SETQ CXF '(-1))
         (SETQ N (MINUS N)))))
      (SETQ EP 1)
      (SETQ NM (CDAAR V))
      (SETQ UH (CDAR U))
      (SETQ VH (CDAR V))
      (SETQ UT
              (COND
               ((NEQ N 0)
                ((LAMBDA (G544)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 (CDR U)))
                         (T (POLY-MULTF G544 (CDR U)))))
                 (LIST (CONS (CONS W N) 1))))
               (T (CDR U))))
      (SETQ VT (CDR V))
      (SETQ CX 1)
      (SETQ CUH UH)
      (SETQ CVH VH)
      (SETQ CX (GCDF* UH VH))
      (COND
       ((EQUAL CX 1)
        (PROGN
         (COND
          (*FASTRESVB
           (PROGN
            (PROGN (PRIN2 "Found trivial factor from headpolys only. ") NIL)
            (TERPRI))))))
       (T
        (PROGN
         (COND
          (*FASTRESVB
           (PROGN
            (PROGN
             (PRIN2 "Found factor from headpolys with ")
             (PRIN2 (TERMSF CX))
             (PRIN2 " terms.")
             NIL)
            (TERPRI)
            NIL)))
         (SETQ CXF (SFTO_FCTRF CX)))))
      (SETQ AP
              (ADDF
               ((LAMBDA (G180)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF G180 VT))
                        (T (POLY-MULTF G180 VT))))
                (QUOTF1 UH CX))
               (NEGF
                ((LAMBDA (G182)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF G182 UT))
                         (T (POLY-MULTF G182 UT))))
                 (QUOTF1 VH CX)))))
      (SETQ X (SFTO_*SF2EXB AP W))
      (COND ((CDR CXF) (SETQ X (|SFTO_B:TRY_PREVIOUS_FACTORS| X (CDR CXF)))))
      (SETQ CX (|SFTO_B:COMFAC| X))
      (COND
       ((NEQ CX 1)
        (PROGN
         (SETQ CXF (SFTO_BFAC-MERGE (SFTO_FCTRF CX) CXF))
         (COND
          (*FASTRESVB
           (PROGN
            (PROGN
             (PRIN2 "commom factor cx found. ")
             (PRIN2 (TMSF CX))
             (PRIN2 " terms.")
             NIL)
            (TERPRI)))))))
      (SETQ X (|SFTO_B:CQUOT| X CX))
      (SETQ EP (|SFTO_B:EXTMULT| X EP))
      (SETQ EP (|SFTO_B:TRY_PREVIOUS_FACTORS| EP (CDR CXF)))
      (PROG (J)
        (SETQ J (DIFFERENCE NM 1))
       LAB
        (COND
         ((MINUSP (TIMES (MINUS 1) (DIFFERENCE (PLUS N 1) J))) (RETURN NIL)))
        (PROGN
         (COND
          ((EQUAL (DEGR UT W) J)
           (PROGN
            (SETQ UH
                    (ADDF (CDAR UT)
                          ((LAMBDA (G184)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G184 UH))
                                   (T (POLY-MULTF G184 UH))))
                           (LIST (CONS (CONS W 1) 1)))))
            (COND
             ((NULL (OR (EQUAL CUH 1) (EQUAL CVH 1)))
              (SETQ CUH (GCDF* (CDAR UT) CUH))))
            (SETQ UT (CDR UT))))
          (T
           (SETQ UH
                   ((LAMBDA (G186)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G186 UH))
                            (T (POLY-MULTF G186 UH))))
                    (LIST (CONS (CONS W 1) 1))))))
         (COND
          ((EQUAL (DEGR VT W) J)
           (PROGN
            (SETQ VH
                    (ADDF (CDAR VT)
                          ((LAMBDA (G188)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G188 VH))
                                   (T (POLY-MULTF G188 VH))))
                           (LIST (CONS (CONS W 1) 1)))))
            (COND
             ((NULL (OR (EQUAL CUH 1) (EQUAL CVH 1)))
              (SETQ CVH (GCDF* (CDAR VT) CVH))))
            (SETQ VT (CDR VT))))
          (T
           (SETQ VH
                   ((LAMBDA (G190)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G190 VH))
                            (T (POLY-MULTF G190 VH))))
                    (LIST (CONS (CONS W 1) 1))))))
         (SETQ CX 1)
         (SETQ CX (GCDF* CUH CVH))
         (COND
          ((EQUAL CX 1)
           (PROGN
            (COND
             (*FASTRESVB
              (PROGN
               (PROGN (PRIN2 "Found trivial factor from headpolys only. ") NIL)
               (TERPRI))))))
          (T
           (PROGN
            (COND
             (*FASTRESVB
              (PROGN
               (PROGN
                (PRIN2 "Found factor from headpolys with ")
                (PRIN2 (TERMSF CX))
                (PRIN2 " terms.")
                NIL)
               (TERPRI))))
            (SETQ CXF (SFTO_BFAC-MERGE (SFTO_FCTRF CX) CXF)))))
         (SETQ X
                 (SFTO_*SF2EXB
                  (ADDF
                   ((LAMBDA (G192)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF G192 VT))
                            (T (POLY-MULTF G192 VT))))
                    (QUOTF1 UH CX))
                   (NEGF
                    ((LAMBDA (G194)
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF G194 UT))
                             (T (POLY-MULTF G194 UT))))
                     (QUOTF1 VH CX))))
                  W))
         (SETQ X (|SFTO_B:TRY_PREVIOUS_FACTORS| X (CDR CXF)))
         (SETQ CX (|SFTO_B:COMFAC| X))
         (COND
          ((NEQ CX 1)
           (PROGN
            (COND
             (*FASTRESVB
              (PROGN
               (PROGN
                (PRIN2 "commom factor cx found. ")
                (PRIN2 (TMSF CX))
                (PRIN2 " terms.")
                NIL)
               (TERPRI))))
            (SETQ CXF (SFTO_BFAC-MERGE (SFTO_FCTRF CX) CXF)))))
         (SETQ X (|SFTO_B:CQUOT| X CX))
         (SETQ EP (|SFTO_B:EXTMULT| X EP))
         (SETQ EP (|SFTO_B:TRY_PREVIOUS_FACTORS| EP (CDR CXF)))
         (COND
          ((NEQ J 1)
           (PROGN
            (SETQ CEP (|SFTO_B:COMFAC| EP))
            (COND
             ((NEQ CEP 1)
              (PROGN
               (SETQ CXF (SFTO_BFAC-MERGE (SFTO_FCTRF CEP) CXF))
               (COND
                (*FASTRESVB
                 (PROGN
                  (PROGN
                   (PRIN2 "commom factor cep found. ")
                   (PRIN2 (TMSF CEP))
                   (PRIN2 " terms.")
                   NIL)
                  (TERPRI)))))))
            (SETQ EP (|SFTO_B:CQUOT| EP CEP))))))
        (SETQ J (PLUS2 J (MINUS 1)))
        (GO LAB))
      (COND
       ((NEQ N 0)
        (PROGN
         (SETQ X (SFTO_*SF2EXB U W))
         (SETQ CX (|SFTO_B:COMFAC| X))
         (COND
          ((NEQ CX 1)
           (PROGN
            (COND
             (*FASTRESVB
              (PROGN
               (PROGN
                (PRIN2 "commom factor cx found.")
                (PRIN2 (TMSF CX))
                (PRIN2 " terms")
                NIL)
               (TERPRI))))
            (SETQ CXF (SFTO_BFAC-MERGE (SFTO_FCTRF CX) CXF)))))
         (SETQ X (|SFTO_B:CQUOT| X CX))
         (SETQ EP (|SFTO_B:EXTMULT| X EP))
         (SETQ CEP (|SFTO_B:COMFAC| EP))
         (COND
          ((NEQ CEP 1)
           (PROGN
            (COND
             (*FASTRESVB
              (PROGN
               (PROGN
                (PRIN2 "commom factor cep found. ")
                (PRIN2 (TMSF CEP))
                (PRIN2 " terms.")
                NIL)
               (TERPRI))))
            (SETQ CXF (SFTO_BFAC-MERGE (SFTO_FCTRF CEP) CXF)))))
         (SETQ EP (|SFTO_B:CQUOT| EP CEP))
         (PROG (J)
           (SETQ J 1)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) J)) (RETURN NIL)))
           (PROGN
            (SETQ X
                    (SFTO_*SF2EXB
                     ((LAMBDA (G544)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G544 U))
                              (T (POLY-MULTF G544 U))))
                      (LIST (CONS (CONS W J) 1)))
                     W))
            (SETQ CX (|SFTO_B:COMFAC| X))
            (COND
             ((NEQ CX 1)
              (PROGN
               (COND
                (*FASTRESVB
                 (PROGN
                  (PROGN
                   (PRIN2 "commom factor cx found. ")
                   (PRIN2 (TMSF CX))
                   (PRIN2 " terms.")
                   NIL)
                  (TERPRI))))
               (SETQ CXF (SFTO_BFAC-MERGE (SFTO_FCTRF CX) CXF)))))
            (SETQ X (|SFTO_B:CQUOT| X CX))
            (SETQ EP (|SFTO_B:EXTMULT| X EP))
            (SETQ CEP (|SFTO_B:COMFAC| EP))
            (COND
             ((NEQ CEP 1)
              (PROGN
               (SETQ CXF (SFTO_BFAC-MERGE (SFTO_FCTRF CEP) CXF))
               (COND
                (*FASTRESVB
                 (PROGN
                  (PROGN
                   (PRIN2 "commom factor cep found. ")
                   (PRIN2 (TMSF CEP))
                   (PRIN2 " terms.")
                   NIL)
                  (TERPRI)))))))
            (SETQ EP (|SFTO_B:CQUOT| EP CEP))
            NIL)
           (SETQ J (PLUS2 J 1))
           (GO LAB)))))
      (RETURN
       (COND ((NULL EP) (LIST (LIST 0 1)))
             ((OR (ATOM (CDAR EP)) (ATOM (CAR (CDAR EP))))
              (CONS
               (LIST
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR CXF) (CDAR EP)))
                      (T (POLY-MULTF (CAR CXF) (CDAR EP))))
                1)
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (CDR CXF))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J) (LIST (CAR J) (CDR J))) (CAR J))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (LIST (CAR J) (CDR J))) (CAR J))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
             (T
              (CONS (LIST (CAR CXF) 1)
                    (CONS (LIST (CDAR EP) 1)
                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                            (SETQ J (CDR CXF))
                            (COND ((NULL J) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (J)
                                                (LIST (CAR J) (CDR J)))
                                              (CAR J))
                                             NIL)))
                           LOOPLABEL
                            (SETQ J (CDR J))
                            (COND ((NULL J) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (J) (LIST (CAR J) (CDR J)))
                                      (CAR J))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))))))) 
(PUT 'SFTO_*SF2EXB 'NUMBER-OF-ARGS 2) 
(PUT 'SFTO_*SF2EXB 'DEFINED-ON-LINE '422) 
(PUT 'SFTO_*SF2EXB 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT 'SFTO_*SF2EXB 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SFTO_*SF2EXB (U V)
    (COND
     ((EQUAL (DEGR U V) 0)
      (COND ((NULL U) NIL) (T (CONS (CONS (LIST 0) U) NIL))))
     (T (CONS (CONS (LIST (CDAAR U)) (CDAR U)) (SFTO_*SF2EXB (CDR U) V))))) 
(PUT '|SFTO_B:EXTMULT| 'NUMBER-OF-ARGS 2) 
(PUT '|SFTO_B:EXTMULT| 'DEFINED-ON-LINE '436) 
(PUT '|SFTO_B:EXTMULT| 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT '|SFTO_B:EXTMULT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SFTO_B:EXTMULT| (U V)
    (COND ((OR (NULL U) (NULL V)) NIL) ((EQUAL V 1) U)
          (T
           ((LAMBDA (X)
              (COND
               (X
                (CONS
                 (CONS (CDR X)
                       (COND
                        ((CAR X)
                         (NEGF
                          (COND
                           (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR U) (CDAR V)))
                           (T (POLY-MULTF (CDAR U) (CDAR V))))))
                        (T
                         (COND
                          (*PHYSOP-LOADED (PHYSOP-MULTF (CDAR U) (CDAR V)))
                          (T (POLY-MULTF (CDAR U) (CDAR V)))))))
                 (|SFTO_B:EXTADD| (|SFTO_B:EXTMULT| (LIST (CAR U)) (CDR V))
                  (|SFTO_B:EXTMULT| (CDR U) V))))
               (T
                (|SFTO_B:EXTADD| (|SFTO_B:EXTMULT| (CDR U) V)
                 (|SFTO_B:EXTMULT| (LIST (CAR U)) (CDR V))))))
            (|SFTO_B:ORDEXN| (CAR (CAAR U)) (CAAR V)))))) 
(PUT '|SFTO_B:EXTADD| 'NUMBER-OF-ARGS 2) 
(PUT '|SFTO_B:EXTADD| 'DEFINED-ON-LINE '452) 
(PUT '|SFTO_B:EXTADD| 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT '|SFTO_B:EXTADD| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SFTO_B:EXTADD| (U V)
    (COND ((NULL U) V) ((NULL V) U)
          ((EQUAL (CAAR U) (CAAR V))
           ((LAMBDA (X Y) (COND ((NULL X) Y) (T (CONS (CONS (CAAR U) X) Y))))
            (ADDF (CDAR U) (CDAR V)) (|SFTO_B:EXTADD| (CDR U) (CDR V))))
          ((|SFTO_B:ORDEXP| (CAAR U) (CAAR V))
           (CONS (CAR U) (|SFTO_B:EXTADD| (CDR U) V)))
          (T (CONS (CAR V) (|SFTO_B:EXTADD| U (CDR V)))))) 
(PUT '|SFTO_B:ORDEXP| 'NUMBER-OF-ARGS 2) 
(PUT '|SFTO_B:ORDEXP| 'DEFINED-ON-LINE '464) 
(PUT '|SFTO_B:ORDEXP| 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT '|SFTO_B:ORDEXP| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SFTO_B:ORDEXP| (U V)
    (COND ((NULL U) T) ((GREATERP (CAR U) (CAR V)) T)
          ((EQUAL (CAR U) (CAR V)) (|SFTO_B:ORDEXP| (CDR U) (CDR V))) (T NIL))) 
(PUT '|SFTO_B:ORDEXN| 'NUMBER-OF-ARGS 2) 
(PUT '|SFTO_B:ORDEXN| 'DEFINED-ON-LINE '474) 
(PUT '|SFTO_B:ORDEXN| 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT '|SFTO_B:ORDEXN| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SFTO_B:ORDEXN| (U V)
    (PROG (S X)
     A
      (COND ((NULL V) (RETURN (CONS S (REVERSE (CONS U X)))))
            ((EQUAL U (CAR V)) (RETURN NIL))
            ((AND U (GREATERP U (CAR V)))
             (RETURN (CONS S (APPEND (REVERSE (CONS U X)) V))))
            (T
             (PROGN
              (SETQ X (CONS (CAR V) X))
              (SETQ V (CDR V))
              (SETQ S (NOT S)))))
      (GO A))) 
(PUT '|SFTO_B:COMFAC| 'NUMBER-OF-ARGS 1) 
(PUT '|SFTO_B:COMFAC| 'DEFINED-ON-LINE '493) 
(PUT '|SFTO_B:COMFAC| 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT '|SFTO_B:COMFAC| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |SFTO_B:COMFAC| (U)
    (PROG (*EZGCD X)
      (COND
       (*FASTRESVB (PROGN (PROGN (PRIN2 "entered comfac.") NIL) (TERPRI))))
      (SETQ *EZGCD T)
      (COND ((NULL U) (RETURN 1)))
      (SETQ X (CDAR U))
     A
      (SETQ U (CDR U))
      (COND
       ((NULL U)
        (PROGN
         (COND
          (*FASTRESVB (PROGN (PROGN (PRIN2 "left comfac.") NIL) (TERPRI))))
         (RETURN X))))
      (SETQ X (GCDF* (CDAR U) X))
      (GO A))) 
(PUT '|SFTO_B:CQUOT| 'NUMBER-OF-ARGS 2) 
(PUT '|SFTO_B:CQUOT| 'DEFINED-ON-LINE '515) 
(PUT '|SFTO_B:CQUOT| 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT '|SFTO_B:CQUOT| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SFTO_B:CQUOT| (U V)
    (COND ((NULL U) NIL)
          (T
           (CONS (CONS (CAAR U) (QUOTF1 (CDAR U) V))
                 (|SFTO_B:CQUOT| (CDR U) V))))) 
(PUT '|SFTO_B:TRY_PREVIOUS_FACTORS| 'NUMBER-OF-ARGS 2) 
(PUT '|SFTO_B:TRY_PREVIOUS_FACTORS| 'DEFINED-ON-LINE '521) 
(PUT '|SFTO_B:TRY_PREVIOUS_FACTORS| 'DEFINED-IN-FILE
     'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT '|SFTO_B:TRY_PREVIOUS_FACTORS| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SFTO_B:TRY_PREVIOUS_FACTORS| (U V)
    (PROG (X)
      (COND ((NULL V) (RETURN U)))
     B
      (SETQ X (|SFTO_B:CTRIALDIV| U (CAAR V)))
      (COND ((NULL X) (GO A)))
      (COND
       (*FASTRESVB
        (PROGN (PROGN (PRIN2 "successful trial div. ") NIL) (TERPRI))))
      (SETQ U X)
      (RPLACD (CAR V) (PLUS (CDAR V) 1))
      (GO B)
     A
      (SETQ V (CDR V))
      (COND ((NULL V) (RETURN U)))
      (GO B))) 
(PUT '|SFTO_B:CTRIALDIV| 'NUMBER-OF-ARGS 2) 
(PUT '|SFTO_B:CTRIALDIV| 'DEFINED-ON-LINE '541) 
(PUT '|SFTO_B:CTRIALDIV| 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT '|SFTO_B:CTRIALDIV| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SFTO_B:CTRIALDIV| (U V)
    (PROG (RES W X)
      (COND ((OR (ATOM U) (ATOM (CAR U))) (RETURN (QUOTF1 U V))))
      (COND
       ((SETQ X (QUOTF1 (CDAR U) V))
        (SETQ RES (SETQ W (CONS (CONS (CAAR U) X) NIL))))
       (T (RETURN NIL)))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN RES)))
      (COND
       ((OR (ATOM U) (ATOM (CAR U)))
        (COND ((SETQ X (QUOTF1 U V)) (PROGN (RPLACD W X) (RETURN RES)))
              (T (RETURN NIL)))))
      (COND
       ((SETQ X (QUOTF1 (CDAR U) V)) (RPLACD W (CONS (CONS (CAAR U) X) NIL)))
       (T (RETURN NIL)))
      (SETQ W (CDR W))
      (GO A))) 
(PUT 'SFTO_BFAC-MERGE 'NUMBER-OF-ARGS 2) 
(PUT 'SFTO_BFAC-MERGE 'DEFINED-ON-LINE '566) 
(PUT 'SFTO_BFAC-MERGE 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT 'SFTO_BFAC-MERGE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SFTO_BFAC-MERGE (U V)
    (COND
     ((NULL (CDR V))
      (CONS
       (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR U) (CAR V)))
             (T (POLY-MULTF (CAR U) (CAR V))))
       (CDR U)))
     (T
      (CONS
       (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR U) (CAR V)))
             (T (POLY-MULTF (CAR U) (CAR V))))
       (SFTO_BFAC-MERGE2 (CDR U) (CDR V)))))) 
(PUT 'SFTO_BFAC-MERGE2 'NUMBER-OF-ARGS 2) 
(PUT 'SFTO_BFAC-MERGE2 'DEFINED-ON-LINE '572) 
(PUT 'SFTO_BFAC-MERGE2 'DEFINED-IN-FILE 'REDLOG/RLTOOLS/SFTORESULTANT.RED) 
(PUT 'SFTO_BFAC-MERGE2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SFTO_BFAC-MERGE2 (U V)
    (PROG (X Y R)
      (COND ((NULL U) (RETURN V)))
     C
      (SETQ X (CAR U))
      (SETQ Y V)
     B
      (COND
       ((EQUAL (CAR X) (CAAR Y))
        (PROGN (RPLACD (CAR Y) (PLUS (CDAR Y) (CDR X))) (GO A))))
      (SETQ Y (CDR Y))
      (COND (Y (GO B)))
      (SETQ R (CONS X R))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN (APPEND V R))))
      (GO C))) 
(ENDMODULE) 