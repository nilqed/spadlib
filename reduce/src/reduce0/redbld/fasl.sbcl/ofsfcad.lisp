(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFCAD)) 
(REVISION 'OFSFCAD "$Id: ofsfcad.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFCAD
           "(c) 2000-2009 A. Dolzmann, A. Seidl, T. Sturm, 2010-2017 T. Sturm") 
(FLUID '(OFSF_WD*)) 
(SETQ OFSF_WD* "/tmp/") 
(SWITCH
 (LIST 'RLQEGEN1 'RLCADANS 'RLCADRMWC 'RLCADTREE2DOT 'RLCADTREE2TGF
       'RLCADTREE2GML 'RLCADTREE2GMLXML)) 
(OFF1 'RLQEGEN1) 
(OFF1 'RLCADANS) 
(ON1 'RLCADRMWC) 
(OFF1 'RLCADTREE2DOT) 
(OFF1 'RLCADTREE2TGF) 
(OFF1 'RLCADTREE2GML) 
(ON1 'RLCADTREE2GMLXML) 
(PUT 'CADDATA 'ASSERT_DYNTYPECHK 'CADDATAP) 
(FLAG '(CADDATA) 'ASSERT_DYNTYPE) 
(PUT 'ACELL 'ASSERT_DYNTYPECHK 'ACELLP) 
(FLAG '(ACELL) 'ASSERT_DYNTYPE) 
(PUT 'ATREE 'ASSERT_DYNTYPECHK 'ATREEP) 
(FLAG '(ATREE) 'ASSERT_DYNTYPE) 
(PUT 'CADDATAP 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATAP 'DEFINED-ON-LINE '50) 
(PUT 'CADDATAP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATAP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATAP (S) (AND (VECTORP S) (EQ (GETV S 0) 'CADDATA))) 
(PUT 'ACELLP 'NUMBER-OF-ARGS 1) 
(PUT 'ACELLP 'DEFINED-ON-LINE '53) 
(PUT 'ACELLP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'ACELLP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ACELLP (S) (AND (PAIRP S) (EQ (CAR S) 'ACELL))) 
(PUT 'ATREEP 'NUMBER-OF-ARGS 1) 
(PUT 'ATREEP 'DEFINED-ON-LINE '56) 
(PUT 'ATREEP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'ATREEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ATREEP (S) (AND (PAIRP S) (EQ (CAR S) 'ATREE))) 
(PUT 'OFSF_CADVERBOSEP 'NUMBER-OF-ARGS 0) 
(DE OFSF_CADVERBOSEP NIL (AND *RLVERBOSE *RLCADVERBOSE)) 
(PUT 'OFSF_GCAD 'NUMBER-OF-ARGS 3) 
(DE OFSF_GCAD (PHI PRJORDL AAPLUS)
    (PROG (*RLQEGEN) (SETQ *RLQEGEN T) (RETURN (OFSF_CAD1 PHI PRJORDL AAPLUS)))) 
(PUT 'OFSF_CAD 'NUMBER-OF-ARGS 3) 
(DE OFSF_CAD (PHI PRJORDL AAPLUS) (CDR (OFSF_CAD1 PHI PRJORDL AAPLUS))) 
(PUT 'OFSF_CAD1 'NUMBER-OF-ARGS 3) 
(DE OFSF_CAD1 (PHI PRJORDL AAPLUS)
    (PROG (CD)
      (SETQ CD (OFSF_CADPREPARATION PHI PRJORDL AAPLUS))
      (COND ((OFSF_CADVERBOSEP) (CADDATA_PRINT CD)))
      (COND
       (*RLCADPREPONLY
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T "+ rlcadpreponly is on: Jump into finish.")))
         (RETURN (OFSF_CADFINISH CD)))))
      (OFSF_CADPROJECTION CD)
      (COND
       (*RLCADPROJONLY
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T "+ rlcadprojonly is on: Jump into finish.")))
         (RETURN (OFSF_CADFINISH CD)))))
      (OFSF_CADEXTENSION CD)
      (COND
       (*RLCADEXTONLY
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T "+ rlcadextonly is on: Jump into finish.")))
         (RETURN (OFSF_CADFINISH CD)))))
      (OFSF_SOLUTIONFORMULA CD)
      (RETURN (OFSF_CADFINISH CD)))) 
(PUT 'OFSF_CADPROJ 'NUMBER-OF-ARGS 2) 
(DE OFSF_CADPROJ (PHI PRJORDL) (CDR (OFSF_CADPROJ1 PHI PRJORDL))) 
(PUT 'OFSF_CADPROJ1 'NUMBER-OF-ARGS 2) 
(DE OFSF_CADPROJ1 (PHI PRJORDL)
    (PROG (CD THEO FFL)
      (SETQ CD (OFSF_CADPREPARATION PHI PRJORDL NIL))
      (COND ((OFSF_CADVERBOSEP) (CADDATA_PRINT CD)))
      (OFSF_CADPROJECTION CD)
      (PROG (F)
        (SETQ F (CADDATA_THEO CD))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (MATHPRINT (RL_PREPFOF F))) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (OFSF_RESTOREKORD CD)
      (SETQ THEO
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDATA_THEO CD))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (OFSF_REORDER F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (OFSF_REORDER F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FFL
              (REVERSIP
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J 1)
                 (COND ((MINUSP (DIFFERENCE (CADDATA_R CD) J)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ P (CADDATA_FFJ CD J))
                                    (COND ((NULL P) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (P) (REORDER P))
                                                      (CAR P))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ P (CDR P))
                                    (COND ((NULL P) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (P) (REORDER P)) (CAR P))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))
                                  NIL)))
                LOOPLABEL
                 (SETQ J (PLUS2 J 1))
                 (COND
                  ((MINUSP (DIFFERENCE (CADDATA_R CD) J))
                   (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          (PROG (P FORALL-RESULT FORALL-ENDPTR)
                            (SETQ P (CADDATA_FFJ CD J))
                            (COND ((NULL P) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (P) (REORDER P)) (CAR P))
                                             NIL)))
                           LOOPLABEL
                            (SETQ P (CDR P))
                            (COND ((NULL P) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (P) (REORDER P)) (CAR P))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (RETURN (CONS THEO FFL)))) 
(PUT 'OFSF_CADPNUM 'NUMBER-OF-ARGS 2) 
(DE OFSF_CADPNUM (PHI PRJORDL)
    (PROG (CD DD RESL *RLCADTRIMTREE)
      (SETQ CD (OFSF_CADPREPARATION PHI PRJORDL NIL))
      (COND ((OFSF_CADVERBOSEP) (CADDATA_PRINT CD)))
      (OFSF_CADPROJECTION CD)
      (OFSF_CADEXTENSION CD)
      (OFSF_RESTOREKORD CD)
      (SETQ DD (CADDATA_DD CD))
      (SETQ RESL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH (CADDATA_VARL CD)) I))
                  (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (LENGTH (ATREE_CHILDRENATLEVEL DD I))
                                      NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND
                 ((MINUSP (DIFFERENCE (LENGTH (CADDATA_VARL CD)) I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS (LENGTH (ATREE_CHILDRENATLEVEL DD I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN RESL))) 
(PUT 'OFSF_CADFNUM 'NUMBER-OF-ARGS 2) 
(DE OFSF_CADFNUM (PL PRJORDL)
    (PROG (OLDORDER W FF VARL RESL)
      (SETQ OLDORDER (SETKORDER PRJORDL))
      (SETQ W
              (PROG (FFJ FORALL-RESULT FORALL-ENDPTR)
                (SETQ FFJ PL)
                (COND ((NULL FFJ) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FFJ)
                                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ F FFJ)
                                      (COND ((NULL F) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (F)
                                                          (REORDER
                                                           (CAR (SIMP F))))
                                                        (CAR F))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ F (CDR F))
                                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (F)
                                                  (REORDER (CAR (SIMP F))))
                                                (CAR F))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR FFJ))
                                 NIL)))
               LOOPLABEL
                (SETQ FFJ (CDR FFJ))
                (COND ((NULL FFJ) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FFJ)
                            (PROG (F FORALL-RESULT FORALL-ENDPTR)
                              (SETQ F FFJ)
                              (COND ((NULL F) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (F)
                                                  (REORDER (CAR (SIMP F))))
                                                (CAR F))
                                               NIL)))
                             LOOPLABEL
                              (SETQ F (CDR F))
                              (COND ((NULL F) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (F) (REORDER (CAR (SIMP F))))
                                        (CAR F))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR FFJ))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ FF (REVERSE W))
      (SETQ VARL (REVERSE PRJORDL))
      (SETQ RESL (OFSF_CADFNUM1 FF VARL))
      (SETKORDER OLDORDER)
      (RETURN RESL))) 
(FLAG '(RLCADPNUM) 'OPFN) 
(PUT 'RLCADPNUM 'NUMBER-OF-ARGS 2) 
(PUT 'RLCADPNUM 'DEFINED-ON-LINE '171) 
(PUT 'RLCADPNUM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'RLCADPNUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RLCADPNUM (PHI PRJORDL)
    (CONS 'LIST (OFSF_CADPNUM (RL_SIMP PHI) (CDR PRJORDL)))) 
(FLAG '(RLCADFNUM) 'OPFN) 
(PUT 'RLCADFNUM 'NUMBER-OF-ARGS 2) 
(PUT 'RLCADFNUM 'DEFINED-ON-LINE '176) 
(PUT 'RLCADFNUM 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'RLCADFNUM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RLCADFNUM (PL PRJORDL)
    (PROG ()
      (SETQ PL
              (PROG (FFJ FORALL-RESULT FORALL-ENDPTR)
                (SETQ FFJ (CDR PL))
                (COND ((NULL FFJ) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (FFJ) (CDR FFJ)) (CAR FFJ))
                                      NIL)))
               LOOPLABEL
                (SETQ FFJ (CDR FFJ))
                (COND ((NULL FFJ) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (FFJ) (CDR FFJ)) (CAR FFJ)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS 'LIST (OFSF_CADFNUM PL (CDR PRJORDL)))))) 
(PUT 'OFSF_CADPREPARATION 'NUMBER-OF-ARGS 3) 
(DE OFSF_CADPREPARATION (PHI PRJORDL AAPLUS)
    (PROG (W OPHI VARL QAL OLDORDER PSI AA RVBL CD R K L)
      (SETQ R 0)
      (SETQ K 0)
      (SETQ L 0)
      (COND (*RLVERBOSE (IOTO_TPRIN2T "+++ Preparation Phase")))
      (COND
       (*RLCADDECDEG
        (PROGN
         (SETQ W (OFSF_DECDEG0 PHI))
         (SETQ PHI (CAR W))
         (COND
          (*RLVERBOSE
           (PROGN
            (IOTO_PRIN2 "+ decrease degrees: ")
            (IOTO_PRIN2T
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (CDR W))
              STARTOVER
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (X) (LIST "(" (CAR X) "^" (CDR X) ")"))
                        (CAR X)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ X (CDR X))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (X) (LIST "(" (CAR X) "^" (CDR X) ")"))
                        (CAR X)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ X (CDR X))
               (GO LOOPLABEL)))))))))
      (SETQ OPHI (SETQ PHI (CL_PNF PHI)))
      (PROG (G289)
        (SETQ G289 (OFSF_MKVARL PHI))
        (SETQ VARL (CAR G289))
        (SETQ QAL (CDR G289))
        (RETURN G289))
      (COND
       (PRJORDL
        (PROGN
         (SETQ VARL PRJORDL)
         (SETQ QAL
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X QAL)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X)
                                       (PROGN
                                        (SETQ W
                                                (PROG1 (CAR PRJORDL)
                                                  (SETQ PRJORDL
                                                          (CDR PRJORDL))))
                                        (CONS W (CDR X))))
                                     (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (X)
                               (PROGN
                                (SETQ W
                                        (PROG1 (CAR PRJORDL)
                                          (SETQ PRJORDL (CDR PRJORDL))))
                                (CONS W (CDR X))))
                             (CAR X))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (SETQ R (LENGTH VARL))
      (SETQ K (DIFFERENCE R (LENGTH QAL)))
      (SETQ OLDORDER (SETKORDER VARL))
      (COND (*RLVERBOSE (IOTO_PRIN2T (LIST "+ Kernel order set to " VARL))))
      (SETQ VARL (REVERSE VARL))
      (SETQ QAL (REVERSIP QAL))
      (SETQ PHI (OFSF_REORDER PHI))
      (SETQ PSI (CL_MATRIX PHI))
      (SETQ AA
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CL_TERML PHI))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) F) (CAR F)) NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (F) F) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (AAPLUS
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_PRIN2T
            (LIST "+++ Adding " (LENGTH AAPLUS) " projection polynomials."))))
         (SETQ AAPLUS
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F AAPLUS)
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (F) (REORDER F)) (CAR F))
                                         NIL)))
                  LOOPLABEL
                   (SETQ F (CDR F))
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (F) (REORDER F)) (CAR F)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (SETQ RVBL (REVERSIP (OFSF_CADVBL PHI)))
      (SETQ L (COND ((CDR RVBL) (PLUS K (LENGTH (CAR (CDR RVBL))))) (T 0)))
      (SETQ CD (CADDATA_MKBLANK))
      (CADDATA_PUTPHI CD PHI)
      (CADDATA_PUTK CD K)
      (CADDATA_PUTR CD R)
      (CADDATA_PUTVARL CD VARL)
      (CADDATA_PUTQAL CD QAL)
      (CADDATA_PUTPSI CD PSI)
      (CADDATA_PUTOLDORDER CD OLDORDER)
      (CADDATA_PUTOPHI CD OPHI)
      (CADDATA_PUTTHEO CD NIL)
      (CADDATA_PUTL CD L)
      (CADDATA_PUTAA CD AA)
      (CADDATA_PUTAAPLUS CD AAPLUS)
      (RETURN CD))) 
(PUT 'OFSF_CADPROJECTION 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADPROJECTION (CD)
    (PROG (R FF)
      (SETQ R (CADDATA_R CD))
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T (LIST "+++ Projection Phase"))
         (IOTO_PRIN2T
          (LIST "+ projection order: " (REVERSE (CADDATA_VARL CD)))))))
      (OFSF_CADPROJECTION1 CD)
      (SETQ FF (CADDATA_FF CD))
      (COND
       (*RLVERBOSE
        (COND
         (*RLCADPROJONLY
          (PROG (I)
            (SETQ I 1)
           LAB
            (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
            (PROGN
             (IOTO_TPRIN2T
              (LIST "+ projection factors of level " (PLUS (DIFFERENCE R I) 1)
                    ":"))
             (MATHPRINT
              (CONS 'LIST
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F (GETV FF (PLUS (DIFFERENCE R I) 1)))
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (F) (PREPF F)) (CAR F))
                                            NIL)))
                     LOOPLABEL
                      (SETQ F (CDR F))
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (F) (PREPF F)) (CAR F)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))
            (SETQ I (PLUS2 I 1))
            (GO LAB)))
         (T
          (PROGN
           (IOTO_TPRIN2 "+ number of projection factors of level r,...,1: ")
           (PROG (I)
             (SETQ I 0)
            LAB
             (COND ((MINUSP (DIFFERENCE (DIFFERENCE R 2) I)) (RETURN NIL)))
             (IOTO_PRIN2 (LIST (LENGTH (GETV FF (DIFFERENCE R I))) ","))
             (SETQ I (PLUS2 I 1))
             (GO LAB))
           (IOTO_PRIN2T (LENGTH (GETV FF 1))))))))
      (RETURN NIL))) 
(PUT 'OFSF_CADEXTENSION 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADEXTENSION (CD)
    (PROG (DD R)
      (SETQ R 0)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T "+++ Extension Phase")
         (IOTO_TPRIN2T "+ Building partial CAD tree..."))))
      (SETQ DD (OFSF_PARTIALTREE CD))
      (COND
       (*RLVERBOSE
        (PROGN
         (SETQ R (CADDATA_R CD))
         (IOTO_TPRIN2 "+ number of partial CAD tree nodes of level 0,...,r: ")
         (PROG (I)
           (SETQ I 0)
          LAB
           (COND ((MINUSP (DIFFERENCE (DIFFERENCE R 1) I)) (RETURN NIL)))
           (IOTO_PRIN2 (LIST (LENGTH (ATREE_CHILDRENATLEVEL DD I)) ","))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (IOTO_PRIN2T (LENGTH (ATREE_CHILDRENATLEVEL DD R))))))
      (COND
       (*RLCADTREE2DOT
        (ATREE_2DOT DD (LTO_SCONCAT (LIST OFSF_WD* "cadtree.dot")))))
      (COND
       (*RLCADTREE2TGF
        (ATREE_2DOT DD (LTO_SCONCAT (LIST OFSF_WD* "cadtree.tgf")))))
      (COND
       (*RLCADTREE2GML
        (ATREE_2GML DD (LTO_SCONCAT (LIST OFSF_WD* "cadtree.gml")))))
      (CADDATA_PUTDD CD DD)
      (RETURN NIL))) 
(PUT 'OFSF_CADFINISH 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADFINISH (CD)
    (PROG (W THEO PHIPRIME)
      (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "+++ Finish Phase"))))
      (COND
       (*RLQEGEN1
        (PROGN
         (SETQ W
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J 1)
                  STARTOVER
                   (COND
                    ((MINUSP (DIFFERENCE (MIN2 1 (CADDATA_K CD)) J))
                     (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (PROG (F FORALL-RESULT FORALL-ENDPTR)
                             (SETQ F (CADDATA_FFJ CD J))
                             (COND ((NULL F) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (F) (LIST 'NEQ F NIL))
                                               (CAR F))
                                              NIL)))
                            LOOPLABEL
                             (SETQ F (CDR F))
                             (COND ((NULL F) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (F) (LIST 'NEQ F NIL)) (CAR F))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ J (PLUS2 J 1))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND
                    ((MINUSP (DIFFERENCE (MIN2 1 (CADDATA_K CD)) J))
                     (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (PROG (F FORALL-RESULT FORALL-ENDPTR)
                             (SETQ F (CADDATA_FFJ CD J))
                             (COND ((NULL F) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (F) (LIST 'NEQ F NIL))
                                               (CAR F))
                                              NIL)))
                            LOOPLABEL
                             (SETQ F (CDR F))
                             (COND ((NULL F) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (F) (LIST 'NEQ F NIL)) (CAR F))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ J (PLUS2 J 1))
                   (GO LOOPLABEL)))
         (CADDATA_PUTTHEO CD (APPEND W (CADDATA_THEO CD))))))
      (OFSF_RESTOREKORD CD)
      (SETQ THEO
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADDATA_THEO CD))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (OFSF_REORDER F)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (OFSF_REORDER F)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND (*RLQEGEN (SETQ THEO (OFSF_THSIMPL THEO))))
      (SETQ PHIPRIME (CADDATA_PHIPRIME CD))
      (COND
       ((NEQ PHIPRIME 'UNDEFINED)
        (RETURN
         (CONS THEO (CL_SIMPL (OFSF_REORDER PHIPRIME) THEO (MINUS 1))))))
      (RETURN (CONS THEO (CADDATA_OPHI CD))))) 
(PUT 'OFSF_MKVARL 'NUMBER-OF-ARGS 1) 
(DE OFSF_MKVARL (F)
    (PROG (VL QAL)
      (SETQ VL (CL_FVARL1 F))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           ((LAMBDA (X) (OR (EQ X 'EX) (EQ X 'ALL)))
            (COND ((ATOM F) F) (T (CAR F)))))
          (RETURN NIL)))
        (PROGN
         (SETQ QAL (CONS (CONS (CADR F) (COND ((ATOM F) F) (T (CAR F)))) QAL))
         (SETQ VL (CONS (CADR F) VL))
         (SETQ F (CADDR F)))
        (GO WHILELABEL))
      (RETURN (CONS VL QAL)))) 
(PUT 'OFSF_RESTOREKORD 'NUMBER-OF-ARGS 1) 
(DE OFSF_RESTOREKORD (CD)
    (PROG (OLDORDER)
      (SETQ OLDORDER (CADDATA_OLDORDER CD))
      (COND ((NEQ OLDORDER 'UNDEFINED) (SETKORDER OLDORDER)))
      (COND (*RLVERBOSE (IOTO_TPRIN2T "+ Kernel order was restored.")))
      (RETURN NIL))) 
(PUT 'OFSF_REORDERL 'NUMBER-OF-ARGS 1) 
(DE OFSF_REORDERL (FL)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F FL)
      (COND ((NULL F) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (F) (OFSF_REORDER F)) (CAR F)) NIL)))
     LOOPLABEL
      (SETQ F (CDR F))
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (F) (OFSF_REORDER F)) (CAR F)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OFSF_REORDER 'NUMBER-OF-ARGS 1) 
(DE OFSF_REORDER (F)
    (CL_APPLY2ATS F
                  (FUNCTION
                   (LAMBDA (ATF) (LIST (CAR ATF) (REORDER (CADR ATF)) NIL))))) 
(PUT 'ACELL_MK 'NUMBER-OF-ARGS 5) 
(DE ACELL_MK (IDX SP TV DESC TL) (LIST 'ACELL IDX SP TV DESC TL)) 
(PUT 'ACELL_GETIDX 'NUMBER-OF-ARGS 1) 
(DE ACELL_GETIDX (C) (NTH C 2)) 
(PUT 'ACELL_GETSP 'NUMBER-OF-ARGS 1) 
(DE ACELL_GETSP (C) (NTH C 3)) 
(PUT 'ACELL_GETTV 'NUMBER-OF-ARGS 1) 
(DE ACELL_GETTV (C) (NTH C 4)) 
(PUT 'ACELL_GETDESC 'NUMBER-OF-ARGS 1) 
(DE ACELL_GETDESC (C) (NTH C 5)) 
(PUT 'ACELL_GETTL 'NUMBER-OF-ARGS 1) 
(DE ACELL_GETTL (C) (NTH C 6)) 
(PUT 'ACELL_SRI 'NUMBER-OF-ARGS 1) 
(DE ACELL_SRI (C)
    ((LAMBDA (TL)
       (OR (ATSOC 'ROOT TL) (ATSOC 'BETWEEN TL) (ATSOC 'BELOW TL)
           (ATSOC 'ABOVE TL) (ATSOC 'ARBITRARY TL)))
     (ACELL_GETTL C))) 
(PUT 'ACELL_PUTSP 'NUMBER-OF-ARGS 2) 
(DE ACELL_PUTSP (C SP) (SETCAR (PNTH C 3) SP)) 
(PUT 'ACELL_PUTTV 'NUMBER-OF-ARGS 2) 
(DE ACELL_PUTTV (C TV) (SETCAR (PNTH C 4) TV)) 
(PUT 'ACELL_PUTDESC 'NUMBER-OF-ARGS 2) 
(DE ACELL_PUTDESC (C DESC) (SETCAR (PNTH C 5) DESC)) 
(PUT 'ACELL_PUTTL 'NUMBER-OF-ARGS 2) 
(DE ACELL_PUTTL (C TL) (SETCAR (PNTH C 6) TL)) 
(PUT 'ACELL_ADDTAGIP 'NUMBER-OF-ARGS 2) 
(DE ACELL_ADDTAGIP (C TG) (SETCAR (PNTH C 6) (CONS TG (NTH C 6)))) 
(PUT 'ACELL_TVASSTRING 'NUMBER-OF-ARGS 1) 
(DE ACELL_TVASSTRING (C)
    (PROG (TV)
      (SETQ TV (ACELL_GETTV C))
      (COND ((EQ TV 'TRUE) (RETURN "T")))
      (COND ((EQ TV 'FALSE) (RETURN "F")))
      (RETURN "?"))) 
(PUT 'ACELL_MED 'NUMBER-OF-ARGS 1) 
(DE ACELL_MED (C) (IV_MED (ANU_IV (CAR (ACELL_GETSP C))))) 
(PUT 'ACELL_SORTFN 'NUMBER-OF-ARGS 2) 
(DE ACELL_SORTFN (C1 C2) (RAT_LEQ (ACELL_MED C1) (ACELL_MED C2))) 
(PUT 'OFSF_CADFNUM1 'NUMBER-OF-ARGS 2) 
(DE OFSF_CADFNUM1 (FF VARL)
    (PROG (HH W)
      (SETQ HH
              (PROG (FFJ FORALL-RESULT FORALL-ENDPTR)
                (SETQ FFJ FF)
                (COND ((NULL FFJ) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (FFJ)
                                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ F FFJ)
                                      (COND ((NULL F) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (F)
                                                          (CONS 'DUMMYTAG F))
                                                        (CAR F))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ F (CDR F))
                                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (F) (CONS 'DUMMYTAG F))
                                                (CAR F))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR FFJ))
                                 NIL)))
               LOOPLABEL
                (SETQ FFJ (CDR FFJ))
                (COND ((NULL FFJ) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (FFJ)
                            (PROG (F FORALL-RESULT FORALL-ENDPTR)
                              (SETQ F FFJ)
                              (COND ((NULL F) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (F) (CONS 'DUMMYTAG F))
                                                (CAR F))
                                               NIL)))
                             LOOPLABEL
                              (SETQ F (CDR F))
                              (COND ((NULL F) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (F) (CONS 'DUMMYTAG F))
                                        (CAR F))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR FFJ))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (OFSF_FULLTREE (LTO_LIST2VECTOR (CONS NIL HH)) VARL))
      (RETURN
       (PROG (I FORALL-RESULT FORALL-ENDPTR)
         (SETQ I 0)
         (COND ((MINUSP (DIFFERENCE (LENGTH VARL) I)) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS (LENGTH (ATREE_CHILDRENATLEVEL W I)) NIL)))
        LOOPLABEL
         (SETQ I (PLUS2 I 1))
         (COND ((MINUSP (DIFFERENCE (LENGTH VARL) I)) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS (LENGTH (ATREE_CHILDRENATLEVEL W I)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'OFSF_FULLTREE 'NUMBER-OF-ARGS 2) 
(DE OFSF_FULLTREE (HH VARL)
    (PROG (BASECELL)
      (SETQ BASECELL (ACELL_MK 0 NIL NIL NIL NIL))
      (RETURN (OFSF_FTOC BASECELL HH VARL)))) 
(PUT 'OFSF_FTOC 'NUMBER-OF-ARGS 3) 
(DE OFSF_FTOC (BASECELL HH VARL)
    (PROG (SP XJ CELL TREEL IRI NCBUFFER RES R K J)
      (SETQ R 0)
      (SETQ K 0)
      (SETQ J 0)
      (SETQ SP (ACELL_GETSP BASECELL))
      (SETQ J (PLUS (LENGTH SP) 1))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(" (DIFFERENCE J 1)))))
      (SETQ R (LENGTH VARL))
      (SETQ K R)
      (COND
       ((EQN (DIFFERENCE J 1) R)
        (PROGN
         (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 ")")))
         (RETURN (ATREE_MK BASECELL)))))
      (SETQ XJ (NTH VARL J))
      (SETQ IRI (OFSF_IRIPREPARE (GETV HH J) XJ SP VARL))
      (SETQ NCBUFFER (NCB_INIT))
      (COND (NIL NIL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (SETQ CELL (OFSF_NEXTCELL NCBUFFER SP IRI XJ J K)))
          (RETURN NIL)))
        (PROG (W1)
          (SETQ W1 (OFSF_FTOC CELL HH VARL))
          (SETQ TREEL (CONS W1 TREEL))
          (RETURN W1))
        (GO WHILELABEL))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 ")")))
      (SETQ RES (ATREE_MK BASECELL))
      (ATREE_SETCHILDL RES TREEL)
      (RETURN RES))) 
(PUT 'OFSF_PARTIALTREE 'NUMBER-OF-ARGS 1) 
(DE OFSF_PARTIALTREE (CD)
    (PROG (BASECELL)
      (SETQ BASECELL (ACELL_MK 0 NIL NIL NIL NIL))
      (RETURN (OFSF_PTOC BASECELL (CL_NNF (CADDATA_PSI CD)) CD)))) 
(PUT 'OFSF_PTOC 'NUMBER-OF-ARGS 3) 
(DE OFSF_PTOC (BASECELL PSI CD)
    (PROG (SP VARL XJ CELL TREE TREEL QJ NEUTRAL IRI NCBUFFER RES TV J R K)
      (SETQ J 0)
      (SETQ R 0)
      (SETQ K 0)
      (SETQ SP (ACELL_GETSP BASECELL))
      (SETQ J (PLUS (LENGTH SP) 1))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(" (DIFFERENCE J 1)))))
      (SETQ R (CADDATA_R CD))
      (SETQ VARL (CADDATA_VARL CD))
      (COND
       ((EQN (DIFFERENCE J 1) R)
        (PROGN
         (ACELL_PUTTV BASECELL (OFSF_EVALQFF PSI SP VARL))
         (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 ")")))
         (RETURN (ATREE_MK BASECELL)))))
      (COND (NIL NIL))
      (COND
       (*RLCADTE
        (PROGN
         (SETQ PSI (OFSF_TRIALEVAL PSI SP))
         (COND
          ((MEMQ PSI '(TRUE FALSE))
           (PROGN
            (ACELL_PUTTV BASECELL PSI)
            (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 ":T)")))
            (RETURN (ATREE_MK BASECELL))))))))
      (SETQ K (CADDATA_K CD))
      (SETQ XJ (NTH VARL J))
      (SETQ IRI (OFSF_IRIPREPARE (CADDATA_HHJ CD J) XJ SP VARL))
      (SETQ NCBUFFER (NCB_INIT))
      (COND
       ((AND (LEQ 1 J) (LEQ J K))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (SETQ CELL (OFSF_NEXTCELL NCBUFFER SP IRI XJ J K)))
             (RETURN NIL)))
           (COND
            ((OFSF_ISWHITECELL CELL CD)
             (PROGN
              (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(" J ":W)"))))))
            (T
             (PROG (W1)
               (SETQ W1 (OFSF_PTOC CELL PSI CD))
               (SETQ TREEL (CONS W1 TREEL))
               (RETURN W1))))
           (GO WHILELABEL))
         (COND
          ((NULL TREEL) (REDERR "GCAD: stack full of white cells occured.")))
         (SETQ TREEL (SORT TREEL (FUNCTION ATREE_SORTFN)))
         (SETQ RES (ATREE_MK BASECELL))
         (ATREE_SETCHILDL RES TREEL)
         (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 ")")))
         (RETURN (OFSF_PBFVS RES TREEL)))))
      (SETQ QJ (CDR (NTH (CADDATA_QAL CD) (DIFFERENCE J K))))
      (SETQ NEUTRAL (COND ((EQ QJ 'ALL) 'TRUE) (T 'FALSE)))
      (SETQ TV NEUTRAL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (EQ TV NEUTRAL)
                (SETQ CELL (OFSF_NEXTCELL NCBUFFER SP IRI XJ J K))))
          (RETURN NIL)))
        (PROGN
         (SETQ TREE (OFSF_PTOC CELL PSI CD))
         (PROGN (SETQ TREEL (CONS TREE TREEL)) TREE)
         (SETQ TV (ACELL_GETTV (ATREE_ROOTCELL TREE))))
        (GO WHILELABEL))
      (ACELL_PUTTV BASECELL TV)
      (SETQ RES (ATREE_MK BASECELL))
      (COND
       (*RLCADANS
        (PROGN
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (SETQ CELL (OFSF_NEXTCELL NCBUFFER SP IRI XJ J K)))
             (RETURN NIL)))
           (PROG (W1)
             (SETQ W1 (ATREE_MK CELL))
             (SETQ TREEL (CONS W1 TREEL))
             (RETURN W1))
           (GO WHILELABEL))
         (SETQ TREEL (SORT TREEL (FUNCTION ATREE_SORTFN)))
         (OFSF_ADDANSWERS BASECELL TREEL J CD)
         (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 ")")))
         (ATREE_SETCHILDL RES TREEL)
         (RETURN RES))))
      (COND
       ((NOT *RLCADTRIMTREE)
        (PROGN
         (SETQ TREEL (SORT TREEL (FUNCTION ATREE_SORTFN)))
         (ATREE_SETCHILDL RES TREEL))))
      (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 ")")))
      (RETURN RES))) 
(PUT 'OFSF_ISWHITECELL 'NUMBER-OF-ARGS 2) 
(DE OFSF_ISWHITECELL (CELL CD)
    (PROG (THETA SP)
      (COND
       ((OR (EQ (CADDATA_THEO CD) 'UNDEFINED) (NOT *RLCADRMWC)) (RETURN NIL)))
      (SETQ THETA
              ((LAMBDA (G291)
                 (COND ((AND G291 (CDR G291)) (CONS 'AND G291))
                       ((NULL G291) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                       (T (CAR G291))))
               (CADDATA_THEO CD)))
      (SETQ SP (ACELL_GETSP CELL))
      (SETQ THETA (OFSF_TRIALEVAL THETA SP))
      (COND ((EQ THETA 'FALSE) (RETURN T)))
      (RETURN NIL))) 
(PUT 'OFSF_TRIALEVAL 'NUMBER-OF-ARGS 2) 
(DE OFSF_TRIALEVAL (PSI SP)
    (CL_SIMPL
     (CL_APPLY2ATS1 PSI
                    (FUNCTION
                     (LAMBDA (ATF SP)
                       (LIST (CAR ATF) (OFSF_TRIALEVALSGNF (CADR ATF) SP)
                             NIL)))
                    (LIST SP))
     NIL (MINUS 1))) 
(PUT 'OFSF_PBFVS 'NUMBER-OF-ARGS 2) 
(DE OFSF_PBFVS (BTR TREEL)
    (PROG (TV C W)
      (COND ((NOT *RLCADPBFVS) (RETURN BTR)))
      (COND (NIL NIL))
      (SETQ W (PROG1 (CAR TREEL) (SETQ TREEL (CDR TREEL))))
      (SETQ TV (ACELL_GETTV (ATREE_ROOTCELL W)))
      (COND ((AND (NEQ TV 'TRUE) (NEQ TV 'FALSE)) (RETURN BTR)))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND C TREEL)) (RETURN NIL)))
        (PROGN
         (SETQ W (PROG1 (CAR TREEL) (SETQ TREEL (CDR TREEL))))
         (COND ((NEQ TV (ACELL_GETTV (ATREE_ROOTCELL W))) (SETQ C NIL))))
        (GO WHILELABEL))
      (COND
       (C
        (PROGN
         (ACELL_PUTTV (ATREE_ROOTCELL BTR) TV)
         (COND (*RLCADTRIMTREE (ATREE_SETCHILDL BTR NIL))))))
      (RETURN BTR))) 
(PUT 'OFSF_ADDANSWERS 'NUMBER-OF-ARGS 4) 
(DE OFSF_ADDANSWERS (BASECELL TREEL J CD)
    (PROG (K L)
      (SETQ K 0)
      (SETQ L 0)
      (SETQ K (CADDATA_K CD))
      (SETQ L (CADDATA_L CD))
      (COND
       ((AND (LEQ (PLUS K 1) J) (LEQ J L))
        (PROGN (OFSF_ADDROOTINFO TREEL (GETV (CADDATA_HHTAGS CD) J)) NIL))))) 
(PUT 'OFSF_ADDROOTINFO 'NUMBER-OF-ARGS 2) 
(DE OFSF_ADDROOTINFO (TREEL HHTAGS)
    (PROG (RNL RTG LTG)
      (COND (NIL NIL))
      (COND ((NULL (CDR TREEL)) (RETURN NIL)))
      (SETQ RNL
              (PROG (TAG FORALL-RESULT FORALL-ENDPTR)
                (SETQ TAG HHTAGS)
                (COND ((NULL TAG) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (TAG) (CONS TAG 0)) (CAR TAG))
                                      NIL)))
               LOOPLABEL
                (SETQ TAG (CDR TAG))
                (COND ((NULL TAG) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (TAG) (CONS TAG 0)) (CAR TAG)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RTG
              (OFSF_ADDROOTINFO0DIM (ATREE_ROOTCELL (CADR TREEL)) RNL HHTAGS))
      (ACELL_PUTTL (ATREE_ROOTCELL (CAR TREEL)) (LIST 'BELOW RTG))
      (SETQ TREEL (CDDR TREEL))
      (SETQ LTG RTG)
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR TREEL)) (RETURN NIL)))
        (PROGN
         (SETQ RTG
                 (OFSF_ADDROOTINFO0DIM (ATREE_ROOTCELL (CADR TREEL)) RNL
                  HHTAGS))
         (ACELL_PUTTL (ATREE_ROOTCELL (CAR TREEL)) (LIST 'BETWEEN LTG RTG))
         (SETQ TREEL (CDDR TREEL))
         (SETQ LTG RTG))
        (GO WHILELABEL))
      (ACELL_PUTTL (ATREE_ROOTCELL (CAR TREEL)) (LIST 'ABOVE LTG)))) 
(PUT 'OFSF_ADDROOTINFO0DIM 'NUMBER-OF-ARGS 3) 
(DE OFSF_ADDROOTINFO0DIM (CELL RNL HHTAGS)
    (PROG (TL RI)
      (SETQ TL (ACELL_GETTL CELL))
      (OFSF_RNLINC RNL TL)
      (SETQ RI
              (CONS 'ROOT
                    (PROG (TAG FORALL-RESULT FORALL-ENDPTR)
                      (SETQ TAG (INTERSECTION HHTAGS TL))
                      (COND ((NULL TAG) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (TAG)
                                          (CONS TAG (CDR (ATSOC TAG RNL))))
                                        (CAR TAG))
                                       NIL)))
                     LOOPLABEL
                      (SETQ TAG (CDR TAG))
                      (COND ((NULL TAG) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (TAG) (CONS TAG (CDR (ATSOC TAG RNL))))
                                (CAR TAG))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (ACELL_PUTTL CELL RI)
      (RETURN RI))) 
(PUT 'OFSF_RNLINC 'NUMBER-OF-ARGS 2) 
(DE OFSF_RNLINC (RNL TL)
    (PROG (RN)
      (SETQ RN RNL)
     LAB
      (COND ((NULL RN) (RETURN NIL)))
      ((LAMBDA (RN) (COND ((MEMQ (CAR RN) TL) (SETCDR RN (PLUS (CDR RN) 1)))))
       (CAR RN))
      (SETQ RN (CDR RN))
      (GO LAB))) 
(PUT 'OFSF_IRIPREPARE 'NUMBER-OF-ARGS 4) 
(DE OFSF_IRIPREPARE (HHJ XJ SP VARL)
    (PROG (W)
      (SETQ W
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR HHJ)
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PR)
                                    (CONS (AEX_FROMSF (CDR PR))
                                          (LIST (CAR PR))))
                                  (CAR PR))
                                 NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (PR)
                            (CONS (AEX_FROMSF (CDR PR)) (LIST (CAR PR))))
                          (CAR PR))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W
              (PROG (TAE FORALL-RESULT FORALL-ENDPTR)
                (SETQ TAE W)
                (COND ((NULL TAE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (TAE)
                                    (CONS (OFSF_SUBSP (CAR TAE) SP VARL)
                                          (CDR TAE)))
                                  (CAR TAE))
                                 NIL)))
               LOOPLABEL
                (SETQ TAE (CDR TAE))
                (COND ((NULL TAE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (TAE)
                            (CONS (OFSF_SUBSP (CAR TAE) SP VARL) (CDR TAE)))
                          (CAR TAE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W
              (PROG (TAE FORALL-RESULT FORALL-ENDPTR)
                (SETQ TAE W)
                (COND ((NULL TAE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (TAE)
                                    (CONS (AEX_MKLCNT (AEX_REDUCE (CAR TAE)))
                                          (CDR TAE)))
                                  (CAR TAE))
                                 NIL)))
               LOOPLABEL
                (SETQ TAE (CDR TAE))
                (COND ((NULL TAE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (TAE)
                            (CONS (AEX_MKLCNT (AEX_REDUCE (CAR TAE)))
                                  (CDR TAE)))
                          (CAR TAE))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W
              (PROG (TAE FORALL-RESULT FORALL-ENDPTR)
                (SETQ TAE W)
               STARTOVER
                (COND ((NULL TAE) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (TAE)
                           (COND
                            ((NOT (AEX_SIMPLENUMBERP (CAR TAE)))
                             (LIST
                              (CONS (AEX_REDUCE (AEX_SQFREE (CAR TAE) XJ))
                                    (CDR TAE))))))
                         (CAR TAE)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ TAE (CDR TAE))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL TAE) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (TAE)
                           (COND
                            ((NOT (AEX_SIMPLENUMBERP (CAR TAE)))
                             (LIST
                              (CONS (AEX_REDUCE (AEX_SQFREE (CAR TAE) XJ))
                                    (CDR TAE))))))
                         (CAR TAE)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ TAE (CDR TAE))
                (GO LOOPLABEL)))
      (SETQ W (AEX_TGPAIRWISEPRIME W XJ))
      (RETURN (IRI_INIT W XJ)))) 
(PUT 'AEX_TGPAIRWISEPRIME 'NUMBER-OF-ARGS 2) 
(DE AEX_TGPAIRWISEPRIME (TAEL X)
    (PROG (PPRESTLIST)
      (COND ((OR (NULL TAEL) (NULL (CDR TAEL))) (RETURN TAEL)))
      (SETQ PPRESTLIST (AEX_TGPAIRWISEPRIME (CDR TAEL) X))
      (RETURN (AEX_TGPAIRWISEPRIME1 (CONS (CAR TAEL) PPRESTLIST) X)))) 
(PUT 'AEX_TGPAIRWISEPRIME1 'NUMBER-OF-ARGS 2) 
(DE AEX_TGPAIRWISEPRIME1 (TAEL X)
    (PROG (TAE1 TAE2 TAELNEW G TG D1 DEG)
      (SETQ D1 0)
      (SETQ DEG 0)
      (SETQ TAE1 (PROG1 (CAR TAEL) (SETQ TAEL (CDR TAEL))))
      (SETQ D1 (AEX_DEG (CAR TAE1) X))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TAEL (NOT (EQN D1 0)))) (RETURN NIL)))
        (PROGN
         (SETQ TAE2 (PROG1 (CAR TAEL) (SETQ TAEL (CDR TAEL))))
         (SETQ G (AEX_GCD (TAG_O TAE1) (TAG_O TAE2) X))
         (SETQ DEG (AEX_DEG G X))
         (COND
          ((GREATERP DEG 0)
           (PROGN
            (SETQ TAE1 (TAG_MKTAG (AEX_QUOT (TAG_O TAE1) G X) (TAG_T TAE1)))
            (SETQ TAE2 (TAG_MKTAG (AEX_QUOT (TAG_O TAE2) G X) (TAG_T TAE2)))
            (SETQ TG (TAG_MKTAG G (UNION (TAG_T TAE1) (TAG_T TAE2))))
            (SETQ D1 (DIFFERENCE D1 DEG))
            (PROGN (SETQ TAELNEW (CONS TG TAELNEW)) TG))))
         (COND
          ((GREATERP (AEX_DEG (TAG_O TAE2) X) 0)
           (PROGN (SETQ TAELNEW (CONS TAE2 TAELNEW)) TAE2))))
        (GO WHILELABEL))
      (SETQ TAELNEW (REVERSIP TAELNEW))
      (COND ((NOT (EQN D1 0)) (PROGN (SETQ TAELNEW (CONS TAE1 TAELNEW)) TAE1)))
      (RETURN (APPEND TAELNEW TAEL)))) 
(PUT 'CADDATA_MKBLANK 'NUMBER-OF-ARGS 0) 
(DE CADDATA_MKBLANK NIL
    (PROG (CD)
      (SETQ CD (MKVECT 18))
      (PUTV CD 0 'CADDATA)
      (PUTV CD 1 'UNDEFINED)
      (PUTV CD 2 'UNDEFINED)
      (PUTV CD 3 'UNDEFINED)
      (PUTV CD 4 'UNDEFINED)
      (PUTV CD 5 'UNDEFINED)
      (PUTV CD 6 'UNDEFINED)
      (PUTV CD 7 'UNDEFINED)
      (PUTV CD 8 'UNDEFINED)
      (PUTV CD 9 'UNDEFINED)
      (PUTV CD 10 'UNDEFINED)
      (PUTV CD 11 'UNDEFINED)
      (PUTV CD 13 'UNDEFINED)
      (PUTV CD 14 'UNDEFINED)
      (PUTV CD 15 'UNDEFINED)
      (PUTV CD 16 'UNDEFINED)
      (PUTV CD 17 'UNDEFINED)
      (PUTV CD 18 'UNDEFINED)
      (RETURN CD))) 
(PUT 'CADDATA_PHI 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_PHI 'DEFINED-ON-LINE '835) 
(PUT 'CADDATA_PHI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PHI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_PHI (CD) (GETV CD 1)) 
(PUT 'CADDATA_K 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_K 'DEFINED-ON-LINE '836) 
(PUT 'CADDATA_K 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_K 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_K (CD) (GETV CD 2)) 
(PUT 'CADDATA_R 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_R 'DEFINED-ON-LINE '837) 
(PUT 'CADDATA_R 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_R 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_R (CD) (GETV CD 3)) 
(PUT 'CADDATA_VARL 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_VARL 'DEFINED-ON-LINE '838) 
(PUT 'CADDATA_VARL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_VARL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_VARL (CD) (GETV CD 4)) 
(PUT 'CADDATA_QAL 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_QAL 'DEFINED-ON-LINE '839) 
(PUT 'CADDATA_QAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_QAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_QAL (CD) (GETV CD 5)) 
(PUT 'CADDATA_PSI 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_PSI 'DEFINED-ON-LINE '840) 
(PUT 'CADDATA_PSI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PSI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_PSI (CD) (GETV CD 6)) 
(PUT 'CADDATA_FF 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_FF 'DEFINED-ON-LINE '841) 
(PUT 'CADDATA_FF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_FF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_FF (CD) (GETV CD 7)) 
(PUT 'CADDATA_FFL 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_FFL 'DEFINED-ON-LINE '842) 
(PUT 'CADDATA_FFL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_FFL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_FFL (CD) (CDR (VECTOR2LIST (GETV CD 7)))) 
(PUT 'CADDATA_FFJ 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_FFJ 'DEFINED-ON-LINE '843) 
(PUT 'CADDATA_FFJ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_FFJ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_FFJ (CD J) (GETV (GETV CD 7) J)) 
(PUT 'CADDATA_DD 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_DD 'DEFINED-ON-LINE '844) 
(PUT 'CADDATA_DD 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_DD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_DD (CD) (GETV CD 8)) 
(PUT 'CADDATA_PHIPRIME 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_PHIPRIME 'DEFINED-ON-LINE '845) 
(PUT 'CADDATA_PHIPRIME 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PHIPRIME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_PHIPRIME (CD) (GETV CD 9)) 
(PUT 'CADDATA_OLDORDER 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_OLDORDER 'DEFINED-ON-LINE '846) 
(PUT 'CADDATA_OLDORDER 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_OLDORDER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_OLDORDER (CD) (GETV CD 10)) 
(PUT 'CADDATA_OPHI 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_OPHI 'DEFINED-ON-LINE '847) 
(PUT 'CADDATA_OPHI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_OPHI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_OPHI (CD) (GETV CD 11)) 
(PUT 'CADDATA_THEO 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_THEO 'DEFINED-ON-LINE '849) 
(PUT 'CADDATA_THEO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_THEO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_THEO (CD) (GETV CD 13)) 
(PUT 'CADDATA_HH 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_HH 'DEFINED-ON-LINE '850) 
(PUT 'CADDATA_HH 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_HH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_HH (CD) (GETV CD 14)) 
(PUT 'CADDATA_HHJ 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_HHJ 'DEFINED-ON-LINE '851) 
(PUT 'CADDATA_HHJ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_HHJ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_HHJ (CD J) (GETV (GETV CD 14) J)) 
(PUT 'CADDATA_L 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_L 'DEFINED-ON-LINE '852) 
(PUT 'CADDATA_L 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_L 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_L (CD) (GETV CD 15)) 
(PUT 'CADDATA_HHTAGS 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_HHTAGS 'DEFINED-ON-LINE '853) 
(PUT 'CADDATA_HHTAGS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_HHTAGS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_HHTAGS (CD) (GETV CD 16)) 
(PUT 'CADDATA_AA 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_AA 'DEFINED-ON-LINE '854) 
(PUT 'CADDATA_AA 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_AA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_AA (CD) (GETV CD 17)) 
(PUT 'CADDATA_AAPLUS 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_AAPLUS 'DEFINED-ON-LINE '855) 
(PUT 'CADDATA_AAPLUS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_AAPLUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_AAPLUS (CD) (GETV CD 18)) 
(PUT 'CADDATA_BVL 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_BVL 'DEFINED-ON-LINE '857) 
(PUT 'CADDATA_BVL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_BVL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_BVL (CD)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I (PLUS (CADDATA_K CD) 1))
      (COND ((MINUSP (DIFFERENCE (CADDATA_R CD) I)) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS (NTH (CADDATA_VARL CD) I) NIL)))
     LOOPLABEL
      (SETQ I (PLUS2 I 1))
      (COND ((MINUSP (DIFFERENCE (CADDATA_R CD) I)) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS (NTH (CADDATA_VARL CD) I) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CADDATA_FVL 'NUMBER-OF-ARGS 1) 
(PUT 'CADDATA_FVL 'DEFINED-ON-LINE '862) 
(PUT 'CADDATA_FVL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_FVL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CADDATA_FVL (CD)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I 1)
      (COND ((MINUSP (DIFFERENCE (CADDATA_K CD) I)) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR (CONS (NTH (CADDATA_VARL CD) I) NIL)))
     LOOPLABEL
      (SETQ I (PLUS2 I 1))
      (COND ((MINUSP (DIFFERENCE (CADDATA_K CD) I)) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS (NTH (CADDATA_VARL CD) I) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CADDATA_PUTPHI 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTPHI 'DEFINED-ON-LINE '867) 
(PUT 'CADDATA_PUTPHI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTPHI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTPHI (CD PHI) (PUTV CD 1 PHI)) 
(PUT 'CADDATA_PUTK 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTK 'DEFINED-ON-LINE '868) 
(PUT 'CADDATA_PUTK 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTK (CD K) (PUTV CD 2 K)) 
(PUT 'CADDATA_PUTR 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTR 'DEFINED-ON-LINE '869) 
(PUT 'CADDATA_PUTR 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTR (CD R) (PUTV CD 3 R)) 
(PUT 'CADDATA_PUTVARL 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTVARL 'DEFINED-ON-LINE '870) 
(PUT 'CADDATA_PUTVARL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTVARL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTVARL (CD VARL) (PUTV CD 4 VARL)) 
(PUT 'CADDATA_PUTQAL 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTQAL 'DEFINED-ON-LINE '871) 
(PUT 'CADDATA_PUTQAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTQAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTQAL (CD QAL) (PUTV CD 5 QAL)) 
(PUT 'CADDATA_PUTPSI 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTPSI 'DEFINED-ON-LINE '872) 
(PUT 'CADDATA_PUTPSI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTPSI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTPSI (CD PSI) (PUTV CD 6 PSI)) 
(PUT 'CADDATA_PUTFF 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTFF 'DEFINED-ON-LINE '873) 
(PUT 'CADDATA_PUTFF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTFF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTFF (CD FF) (PUTV CD 7 FF)) 
(PUT 'CADDATA_PUTDD 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTDD 'DEFINED-ON-LINE '874) 
(PUT 'CADDATA_PUTDD 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTDD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTDD (CD DD) (PUTV CD 8 DD)) 
(PUT 'CADDATA_PUTPHIPRIME 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTPHIPRIME 'DEFINED-ON-LINE '875) 
(PUT 'CADDATA_PUTPHIPRIME 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTPHIPRIME 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTPHIPRIME (CD PHIPRIME) (PUTV CD 9 PHIPRIME)) 
(PUT 'CADDATA_PUTOLDORDER 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTOLDORDER 'DEFINED-ON-LINE '876) 
(PUT 'CADDATA_PUTOLDORDER 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTOLDORDER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTOLDORDER (CD OLDORDER) (PUTV CD 10 OLDORDER)) 
(PUT 'CADDATA_PUTOPHI 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTOPHI 'DEFINED-ON-LINE '877) 
(PUT 'CADDATA_PUTOPHI 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTOPHI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTOPHI (CD PHI) (PUTV CD 11 PHI)) 
(PUT 'CADDATA_PUTTHEO 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTTHEO 'DEFINED-ON-LINE '879) 
(PUT 'CADDATA_PUTTHEO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTTHEO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTTHEO (CD THEO) (PUTV CD 13 THEO)) 
(PUT 'CADDATA_PUTHH 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTHH 'DEFINED-ON-LINE '880) 
(PUT 'CADDATA_PUTHH 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTHH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTHH (CD HH) (PUTV CD 14 HH)) 
(PUT 'CADDATA_PUTL 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTL 'DEFINED-ON-LINE '881) 
(PUT 'CADDATA_PUTL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTL (CD L) (PUTV CD 15 L)) 
(PUT 'CADDATA_PUTHHTAGS 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTHHTAGS 'DEFINED-ON-LINE '882) 
(PUT 'CADDATA_PUTHHTAGS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTHHTAGS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTHHTAGS (CD HHTAGS) (PUTV CD 16 HHTAGS)) 
(PUT 'CADDATA_PUTAA 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTAA 'DEFINED-ON-LINE '883) 
(PUT 'CADDATA_PUTAA 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTAA 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTAA (CD AA) (PUTV CD 17 AA)) 
(PUT 'CADDATA_PUTAAPLUS 'NUMBER-OF-ARGS 2) 
(PUT 'CADDATA_PUTAAPLUS 'DEFINED-ON-LINE '884) 
(PUT 'CADDATA_PUTAAPLUS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFCAD.RED) 
(PUT 'CADDATA_PUTAAPLUS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CADDATA_PUTAAPLUS (CD AAPLUS) (PUTV CD 18 AAPLUS)) 
(PUT 'CADDATA_PRINT 'NUMBER-OF-ARGS 1) 
(DE CADDATA_PRINT (CD)
    (PROG ()
      (IOTO_PRIN2T "+ BEGIN caddata_print")
      (COND (*RLCADVERBOSE (CADDATA_PRINTALL CD)) (T (CADDATA_PRINTSOME CD)))
      (IOTO_PRIN2T "+ END caddata_print"))) 
(PUT 'CADDATA_PRINTALL 'NUMBER-OF-ARGS 1) 
(DE CADDATA_PRINTALL (CD)
    (PROG ()
      (IOTO_PRIN2T (LIST "phi := " (CADDATA_PHI CD)))
      (IOTO_PRIN2T (LIST "k := " (CADDATA_K CD)))
      (IOTO_PRIN2T (LIST "r := " (CADDATA_R CD)))
      (IOTO_PRIN2T (LIST "varl := " (CADDATA_VARL CD)))
      (IOTO_PRIN2T (LIST "qal := " (CADDATA_QAL CD)))
      (IOTO_PRIN2T (LIST "psi := " (CADDATA_PSI CD)))
      (IOTO_PRIN2T (LIST "ff := " (CADDATA_FF CD)))
      (IOTO_PRIN2T (LIST "dd := " (CADDATA_DD CD)))
      (IOTO_PRIN2T (LIST "phiprime := " (CADDATA_PHIPRIME CD)))
      (IOTO_PRIN2T (LIST "oldorder := " (CADDATA_OLDORDER CD)))
      (IOTO_PRIN2T (LIST "ophi := " (CADDATA_OPHI CD)))
      (IOTO_PRIN2T (LIST "theo := " (CADDATA_THEO CD)))
      (IOTO_PRIN2T (LIST "hh := " (CADDATA_HH CD)))
      (IOTO_PRIN2T (LIST "l := " (CADDATA_L CD)))
      (IOTO_PRIN2T (LIST "hhtags := " (CADDATA_HHTAGS CD)))
      (IOTO_PRIN2T (LIST "aa := " (CADDATA_AA CD)))
      (IOTO_PRIN2T (LIST "aaplus := " (CADDATA_AAPLUS CD))))) 
(PUT 'CADDATA_PRINTSOME 'NUMBER-OF-ARGS 1) 
(DE CADDATA_PRINTSOME (CD)
    (PROG ()
      (COND
       ((NEQ (CADDATA_K CD) 'UNDEFINED)
        (IOTO_PRIN2T (LIST "k := " (CADDATA_K CD)))))
      (COND
       ((NEQ (CADDATA_R CD) 'UNDEFINED)
        (IOTO_PRIN2T (LIST "r := " (CADDATA_R CD)))))
      (COND
       ((NEQ (CADDATA_VARL CD) 'UNDEFINED)
        (IOTO_PRIN2T (LIST "varl := " (CADDATA_VARL CD)))))
      (COND
       ((NEQ (CADDATA_QAL CD) 'UNDEFINED)
        (IOTO_PRIN2T (LIST "qal := " (CADDATA_QAL CD)))))
      (COND
       ((NEQ (CADDATA_OLDORDER CD) 'UNDEFINED)
        (IOTO_PRIN2T (LIST "oldorder := " (CADDATA_OLDORDER CD))))))) 
(PUT 'OFSF_PRINTCADSWITCHES 'NUMBER-OF-ARGS 0) 
(DE OFSF_PRINTCADSWITCHES NIL
    (PROGN
     (IOTO_TPRIN2 "+ begin CAD relevant switches")
     (OFSF_CADSWITCHPRINT *RLVERBOSE)
     (IOTO_PRIN2 "rlverbose;")
     (OFSF_CADSWITCHPRINT *RLCADVERBOSE)
     (IOTO_PRIN2 "rlcadverbose;")
     (OFSF_CADSWITCHPRINT *RLANUEXVERBOSE)
     (IOTO_PRIN2 "rlanuexverbose;")
     (OFSF_CADSWITCHPRINT *RLCADDECDEG)
     (IOTO_PRIN2 "rlcaddecdeg;")
     (OFSF_CADSWITCHPRINT *RLCADTE)
     (IOTO_PRIN2 "rlcadte;")
     (OFSF_CADSWITCHPRINT *RLCADPBFVS)
     (IOTO_PRIN2 "rlcadpbfvs;")
     (OFSF_CADSWITCHPRINT *RLCADTRIMTREE)
     (IOTO_PRIN2 "rlcadtrimtree;")
     (OFSF_CADSWITCHPRINT *RLCADFASTEVAL)
     (IOTO_PRIN2 "rlcadfasteval;")
     (OFSF_CADSWITCHPRINT *RLCADFULLDIMONLY)
     (IOTO_PRIN2 "rlcadfulldimonly;")
     (OFSF_CADSWITCHPRINT *RLCADANS)
     (IOTO_PRIN2 "rlcadans;")
     (OFSF_CADSWITCHPRINT *RLCADTREE2DOT)
     (IOTO_PRIN2 "rlcadtree2dot;")
     (OFSF_CADSWITCHPRINT *RLCADRMWC)
     (IOTO_PRIN2 "rlcadrmwc;")
     (OFSF_CADSWITCHPRINT *RLCADRAWFORMULA)
     (IOTO_PRIN2 "rlcadrawformula;")
     (OFSF_CADSWITCHPRINT *RLCADPREPONLY)
     (IOTO_PRIN2 "rlcadpreponly;")
     (OFSF_CADSWITCHPRINT *RLCADPROJONLY)
     (IOTO_PRIN2 "rlcadprojonly;")
     (OFSF_CADSWITCHPRINT *RLCADEXTONLY)
     (IOTO_PRIN2 "rlcadextonly;")
     (IOTO_TPRIN2 "+ end CAD relevant switches"))) 
(PUT 'OFSF_CADSWITCHPRINT 'NUMBER-OF-ARGS 1) 
(DE OFSF_CADSWITCHPRINT (B)
    (COND (B (IOTO_TPRIN2 "on ")) (T (IOTO_TPRIN2 "off ")))) 
(PUT 'NCB_INIT 'NUMBER-OF-ARGS 0) 
(DE NCB_INIT NIL (LIST NIL)) 
(PUT 'NCB_GET 'NUMBER-OF-ARGS 1) 
(DE NCB_GET (NCB) (PROG (W) (SETQ W (CAR NCB)) (SETCAR NCB NIL) (RETURN W))) 
(PUT 'NCB_PUT 'NUMBER-OF-ARGS 2) 
(DE NCB_PUT (W NCB) (PROGN (COND (NIL NIL)) (SETCAR NCB W) NCB)) 
(PUT 'OFSF_NEXTCELL 'NUMBER-OF-ARGS 6) 
(DE OFSF_NEXTCELL (NCBUFFER SP IRI XJ J K)
    (PROG (CELL TGROOT ROOT W CIND)
      (SETQ CIND 0)
      (SETQ CELL (NCB_GET NCBUFFER))
      (COND
       ((EQ CELL 'FINISHED) (PROGN (NCB_PUT 'FINISHED NCBUFFER) (RETURN NIL))))
      (COND
       (CELL
        (COND
         ((AND *RLCADFULLDIMONLY (GREATERP J K))
          (PROGN (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(" J ":F)"))))))
         ((AND *RLQEGEN1 (NEQ K 0) (EQN J 1))
          (PROGN (COND ((OFSF_CADVERBOSEP) (IOTO_PRIN2 (LIST "(" J ":G)"))))))
         (T (RETURN CELL)))))
      (SETQ TGROOT (IRI_NEXTROOT IRI))
      (SETQ CIND (TIMES 2 (LENGTH (IRI_ROOTL IRI))))
      (COND
       (TGROOT
        (PROGN
         (SETQ ROOT (TAG_O TGROOT))
         (NCB_PUT
          (ACELL_MK (DIFFERENCE CIND 1) (CONS ROOT SP) NIL NIL (TAG_T TGROOT))
          NCBUFFER)
         (SETQ W (IV_LB (ANU_IV ROOT)))
         (RETURN
          (ACELL_MK (DIFFERENCE CIND 2) (CONS (ANU_FROMRAT XJ W) SP) NIL NIL
           NIL)))))
      (NCB_PUT 'FINISHED NCBUFFER)
      (COND
       ((NULL (IRI_ROOTL IRI))
        (RETURN
         (ACELL_MK 0 (CONS (ANU_FROMRAT XJ (RAT_0)) SP) NIL NIL 'ARBITRARY))))
      (SETQ W
              (RAT_MAPMAX
               (PROG (TANU FORALL-RESULT FORALL-ENDPTR)
                 (SETQ TANU (IRI_ROOTL IRI))
                 (COND ((NULL TANU) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (TANU)
                                     (IV_RB (ANU_IV (TAG_O TANU))))
                                   (CAR TANU))
                                  NIL)))
                LOOPLABEL
                 (SETQ TANU (CDR TANU))
                 (COND ((NULL TANU) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (TANU) (IV_RB (ANU_IV (TAG_O TANU))))
                           (CAR TANU))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (RETURN (ACELL_MK CIND (CONS (ANU_FROMRAT XJ W) SP) NIL NIL NIL)))) 
(PUT 'OFSF_SUBSP 'NUMBER-OF-ARGS 3) 
(DE OFSF_SUBSP (AE SP VARL)
    (PROG (X ANU)
      (SETQ SP (REVERSE SP))
      (PROG ()
       WHILELABEL
        (COND ((NOT SP) (RETURN NIL)))
        (PROGN
         (SETQ X (PROG1 (CAR VARL) (SETQ VARL (CDR VARL))))
         (SETQ ANU (PROG1 (CAR SP) (SETQ SP (CDR SP))))
         (SETQ AE (AEX_BIND AE X ANU)))
        (GO WHILELABEL))
      (RETURN AE))) 
(PUT 'OFSF_SUBSP* 'NUMBER-OF-ARGS 2) 
(DE OFSF_SUBSP* (AE SP)
    (PROG (ANU)
      (SETQ SP (REVERSE SP))
      (PROG ()
       WHILELABEL
        (COND ((NOT SP) (RETURN NIL)))
        (PROGN
         (SETQ ANU (PROG1 (CAR SP) (SETQ SP (CDR SP))))
         (SETQ AE (AEX_BIND AE (AEX_MVAR (ANU_DP ANU)) ANU)))
        (GO WHILELABEL))
      (RETURN AE))) 
(PUT 'TAG_MKTAG 'NUMBER-OF-ARGS 2) 
(DE TAG_MKTAG (A TAG) (CONS A TAG)) 
(PUT 'TAG_O 'NUMBER-OF-ARGS 1) 
(DE TAG_O (TI) (CAR TI)) 
(PUT 'TAG_T 'NUMBER-OF-ARGS 1) 
(DE TAG_T (TI) (CDR TI)) 
(PUT 'ATREE_MK 'NUMBER-OF-ARGS 1) 
(DE ATREE_MK (C) (LIST 'ATREE C NIL)) 
(PUT 'ATREE_ROOTCELL 'NUMBER-OF-ARGS 1) 
(DE ATREE_ROOTCELL (TT) (NTH TT 2)) 
(PUT 'ATREE_CHILDL 'NUMBER-OF-ARGS 1) 
(DE ATREE_CHILDL (TT) (NTH TT 3)) 
(PUT 'ATREE_CHILDRENATLEVEL 'NUMBER-OF-ARGS 2) 
(DE ATREE_CHILDRENATLEVEL (TT N)
    (COND ((EQN N 0) (LIST (ATREE_ROOTCELL TT)))
          (T
           (PROG (CHILD FORALL-RESULT FORALL-ENDPTR)
             (SETQ CHILD (ATREE_CHILDL TT))
            STARTOVER
             (COND ((NULL CHILD) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (CHILD)
                        (ATREE_CHILDRENATLEVEL CHILD (DIFFERENCE N 1)))
                      (CAR CHILD)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ CHILD (CDR CHILD))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL CHILD) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (CHILD)
                        (ATREE_CHILDRENATLEVEL CHILD (DIFFERENCE N 1)))
                      (CAR CHILD)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ CHILD (CDR CHILD))
             (GO LOOPLABEL))))) 
(PUT 'ATREE_GETLEAVES 'NUMBER-OF-ARGS 1) 
(DE ATREE_GETLEAVES (TT)
    (COND ((NULL (ATREE_CHILDL TT)) (LIST (ATREE_ROOTCELL TT)))
          (T
           (PROG (C FORALL-RESULT FORALL-ENDPTR)
             (SETQ C (ATREE_CHILDL TT))
            STARTOVER
             (COND ((NULL C) (RETURN NIL)))
             (SETQ FORALL-RESULT ((LAMBDA (C) (ATREE_GETLEAVES C)) (CAR C)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ C (CDR C))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL C) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR ((LAMBDA (C) (ATREE_GETLEAVES C)) (CAR C)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ C (CDR C))
             (GO LOOPLABEL))))) 
(PUT 'ATREE_SETCHILDL 'NUMBER-OF-ARGS 2) 
(DE ATREE_SETCHILDL (TT CL) (SETCAR (PNTH TT 3) CL)) 
(PUT 'ATREE_SORTFN 'NUMBER-OF-ARGS 2) 
(DE ATREE_SORTFN (T1 T2) (ACELL_SORTFN (ATREE_ROOTCELL T1) (ATREE_ROOTCELL T2))) 
(PUT 'ATREE_CELLL 'NUMBER-OF-ARGS 1) 
(DE ATREE_CELLL (TT)
    (COND ((NULL (ATREE_CHILDL TT)) (LIST (ATREE_ROOTCELL TT)))
          (T
           (PROG (CHILD FORALL-RESULT FORALL-ENDPTR)
             (SETQ CHILD (ATREE_CHILDL TT))
            STARTOVER
             (COND ((NULL CHILD) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (CHILD) (ATREE_CELLL CHILD)) (CAR CHILD)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ CHILD (CDR CHILD))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL CHILD) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (CHILD) (ATREE_CELLL CHILD)) (CAR CHILD)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ CHILD (CDR CHILD))
             (GO LOOPLABEL))))) 
(PUT 'ATREE_CELLTVL 'NUMBER-OF-ARGS 1) 
(DE ATREE_CELLTVL (TT)
    (COND
     ((NEQ (ACELL_GETTV (ATREE_ROOTCELL TT)) NIL) (LIST (ATREE_ROOTCELL TT)))
     (T
      (PROG (CHILD FORALL-RESULT FORALL-ENDPTR)
        (SETQ CHILD (ATREE_CHILDL TT))
       STARTOVER
        (COND ((NULL CHILD) (RETURN NIL)))
        (SETQ FORALL-RESULT
                ((LAMBDA (CHILD) (ATREE_CELLTVL CHILD)) (CAR CHILD)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
        (SETQ CHILD (CDR CHILD))
        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
       LOOPLABEL
        (COND ((NULL CHILD) (RETURN FORALL-RESULT)))
        (RPLACD FORALL-ENDPTR
                ((LAMBDA (CHILD) (ATREE_CELLTVL CHILD)) (CAR CHILD)))
        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
        (SETQ CHILD (CDR CHILD))
        (GO LOOPLABEL))))) 
(PUT 'ATREE_PRINT 'NUMBER-OF-ARGS 1) 
(DE ATREE_PRINT (TT)
    (PROG (E)
      (SETQ E (ATREE_PRINT1 TT 0))
     LAB
      (COND ((NULL E) (RETURN NIL)))
      ((LAMBDA (E) (IOTO_TPRIN2T E)) (CAR E))
      (SETQ E (CDR E))
      (GO LAB))) 
(PUT 'ATREE_PRINT1 'NUMBER-OF-ARGS 2) 
(DE ATREE_PRINT1 (TT D)
    (PROG (CHILDL ROOTLABEL W)
      (SETQ CHILDL (ATREE_CHILDL TT))
      (SETQ ROOTLABEL (ATREE_ROOTCELL TT))
      (COND
       ((NULL CHILDL) (RETURN (LIST (LIST (ACELL_TVASSTRING ROOTLABEL))))))
      (SETQ W
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S CHILDL)
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (PROG (L FORALL-RESULT FORALL-ENDPTR)
                             (SETQ L (ATREE_PRINT1 S (PLUS D 1)))
                             (COND ((NULL L) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (L)
                                                 (CONS " " (CONS "   " L)))
                                               (CAR L))
                                              NIL)))
                            LOOPLABEL
                             (SETQ L (CDR L))
                             (COND ((NULL L) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (L) (CONS " " (CONS "   " L)))
                                       (CAR L))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (PROG (L FORALL-RESULT FORALL-ENDPTR)
                             (SETQ L (ATREE_PRINT1 S (PLUS D 1)))
                             (COND ((NULL L) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (L)
                                                 (CONS " " (CONS "   " L)))
                                               (CAR L))
                                              NIL)))
                            LOOPLABEL
                             (SETQ L (CDR L))
                             (COND ((NULL L) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (L) (CONS " " (CONS "   " L)))
                                       (CAR L))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (SETCAR (CAR W) (ACELL_TVASSTRING ROOTLABEL))
      (SETCAR (CDAR W) "---")
      (RETURN W))) 
(PUT 'ATREE_PRINT1RAW 'NUMBER-OF-ARGS 2) 
(DE ATREE_PRINT1RAW (TT D)
    (PROG (CHILDL W)
      (SETQ CHILDL (ATREE_CHILDL TT))
      (COND ((NULL CHILDL) (RETURN (LIST (LIST D)))))
      (SETQ W
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S CHILDL)
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (PROG (L FORALL-RESULT FORALL-ENDPTR)
                             (SETQ L (ATREE_PRINT1RAW S (PLUS D 1)))
                             (COND ((NULL L) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (L) (CONS " " L))
                                               (CAR L))
                                              NIL)))
                            LOOPLABEL
                             (SETQ L (CDR L))
                             (COND ((NULL L) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (L) (CONS " " L)) (CAR L))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (PROG (L FORALL-RESULT FORALL-ENDPTR)
                             (SETQ L (ATREE_PRINT1RAW S (PLUS D 1)))
                             (COND ((NULL L) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (L) (CONS " " L))
                                               (CAR L))
                                              NIL)))
                            LOOPLABEL
                             (SETQ L (CDR L))
                             (COND ((NULL L) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS ((LAMBDA (L) (CONS " " L)) (CAR L))
                                           NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL)))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (SETCAR (CAR W) D)
      (RETURN W))) 
(PUT 'ATREE_PRINTLIN 'NUMBER-OF-ARGS 2) 
(DE ATREE_PRINTLIN (TT D)
    (COND ((NULL (ATREE_CHILDL TT)) (IOTO_PRIN2 (LIST "(" D ")")))
          (T
           (PROGN
            (IOTO_PRIN2 (LIST "(" D))
            (PROG (C)
              (SETQ C (ATREE_CHILDL TT))
             LAB
              (COND ((NULL C) (RETURN NIL)))
              ((LAMBDA (C) (ATREE_PRINTLIN C (PLUS D 1))) (CAR C))
              (SETQ C (CDR C))
              (GO LAB))
            (IOTO_PRIN2 ")"))))) 
(PUT 'ATREE_2DOT 'NUMBER-OF-ARGS 2) 
(DE ATREE_2DOT (TT FILENAME)
    (PROG ()
      (OUT (LIST FILENAME))
      (IOTO_PRIN2T "digraph cadtree {")
      (ATREE_2DOT1 TT NIL)
      (IOTO_PRIN2T "}")
      (SHUT (LIST FILENAME)))) 
(PUT 'ATREE_2DOT1 'NUMBER-OF-ARGS 2) 
(DE ATREE_2DOT1 (TT IDX)
    (PROG (CHILDLIST IDXL W I N)
      (SETQ I 0)
      (SETQ N 0)
      (ATREE_2DOTPRINNODE IDX)
      (ATREE_2DOTNODETAIL (ATREE_ROOTCELL TT))
      (SETQ I 1)
      (SETQ CHILDLIST (ATREE_CHILDL TT))
      (SETQ N (LENGTH CHILDLIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT (LEQ I N)) (RETURN NIL)))
        (PROGN
         (SETQ IDXL (APPEND IDX (LIST I)))
         (ATREE_2DOTPRINNODE IDX)
         (IOTO_PRIN2 "->")
         (ATREE_2DOTPRINNODE IDXL)
         (IOTO_PRIN2T "")
         (SETQ W (PROG1 (CAR CHILDLIST) (SETQ CHILDLIST (CDR CHILDLIST))))
         (ATREE_2DOT1 W IDXL)
         (SETQ I (PLUS I 1)))
        (GO WHILELABEL)))) 
(PUT 'ATREE_2DOTPRINNODE 'NUMBER-OF-ARGS 1) 
(DE ATREE_2DOTPRINNODE (IDXL)
    (PROGN
     (IOTO_PRIN2 "C")
     (PROG (E)
       (SETQ E IDXL)
      LAB
       (COND ((NULL E) (RETURN NIL)))
       ((LAMBDA (E) (IOTO_PRIN2 E)) (CAR E))
       (SETQ E (CDR E))
       (GO LAB)))) 
(PUT 'ATREE_2DOTNODETAIL 'NUMBER-OF-ARGS 1) 
(DE ATREE_2DOTNODETAIL (C)
    (PROG (TV)
      (SETQ TV (ACELL_GETTV C))
      (COND
       ((EQ TV 'TRUE)
        (IOTO_PRIN2T " [label=T shape=circle style=filled color=green]"))
       ((EQ TV 'FALSE)
        (IOTO_PRIN2T " [label=F shape=circle style=filled color=red]"))
       (T
        (IOTO_PRIN2T " [label=\"-\" shape=circle style=filled color=grey]"))))) 
(PUT 'ATREE_2TGF 'NUMBER-OF-ARGS 2) 
(DE ATREE_2TGF (TT FILENAME)
    (PROG ()
      (OUT (LIST FILENAME))
      (ATREE_2TGF_NODES TT 1)
      (IOTO_PRIN2T "#")
      (ATREE_2TGF_EDGES TT 1)
      (SHUT (LIST FILENAME)))) 
(PUT 'ATREE_2TGF_NODES 'NUMBER-OF-ARGS 2) 
(DE ATREE_2TGF_NODES (TT NUMBER)
    (PROG (CHILDLIST)
      (IOTO_PRIN2 NUMBER)
      (ACELL_PRIN (ATREE_ROOTCELL TT))
      (SETQ CHILDLIST (ATREE_CHILDL TT))
      (PROG (CHILD)
        (SETQ CHILD CHILDLIST)
       LAB
        (COND ((NULL CHILD) (RETURN NIL)))
        ((LAMBDA (CHILD)
           (SETQ NUMBER (ATREE_2TGF_NODES CHILD (PLUS NUMBER 1))))
         (CAR CHILD))
        (SETQ CHILD (CDR CHILD))
        (GO LAB))
      (RETURN NUMBER))) 
(PUT 'ATREE_2TGF_EDGES 'NUMBER-OF-ARGS 2) 
(DE ATREE_2TGF_EDGES (TT NUMBER)
    (PROG (CHILDLIST MYNUMBER)
      (SETQ MYNUMBER NUMBER)
      (SETQ CHILDLIST (ATREE_CHILDL TT))
      (PROG (CHILD)
        (SETQ CHILD CHILDLIST)
       LAB
        (COND ((NULL CHILD) (RETURN NIL)))
        ((LAMBDA (CHILD)
           (PROGN
            (IOTO_PRIN2T (LIST MYNUMBER " " (PLUS NUMBER 1)))
            (SETQ NUMBER (ATREE_2TGF_EDGES CHILD (PLUS NUMBER 1)))))
         (CAR CHILD))
        (SETQ CHILD (CDR CHILD))
        (GO LAB))
      (RETURN NUMBER))) 
(PUT 'ACELL_PRIN 'NUMBER-OF-ARGS 1) 
(DE ACELL_PRIN (C)
    (PROG (TMP TV)
      (IOTO_PRIN2 " \"")
      (IOTO_PRIN2 (LIST "idx: " (ACELL_GETIDX C)))
      (SETQ TMP (ACELL_GETTV C))
      (SETQ TV (COND ((EQ TMP 'TRUE) "T") ((EQ TMP 'FALSE) "F") (T "?")))
      (IOTO_PRIN2 (LIST ", tv: " TV))
      (IOTO_PRIN2 (LIST ", desc: " (ACELL_GETDESC C) ", tl: " (ACELL_GETTL C)))
      (IOTO_PRIN2T "\""))) 
(PUT 'ATREE_2GML 'NUMBER-OF-ARGS 2) 
(DE ATREE_2GML (TT FILENAME)
    (PROG ()
      (OUT (LIST FILENAME))
      (IOTO_PRIN2T "Creator \"REDLOG\"")
      (IOTO_PRIN2T "graph [")
      (IOTO_PRIN2T "label \"Graph generated by REDLOG.\"")
      (IOTO_PRIN2T "directed 1")
      (ATREE_2GML_NODES TT 1)
      (ATREE_2GML_EDGES TT 1)
      (IOTO_PRIN2T "]")
      (SHUT (LIST FILENAME)))) 
(PUT 'ATREE_2GML_NODES 'NUMBER-OF-ARGS 2) 
(DE ATREE_2GML_NODES (TT NUMBER)
    (PROG (CHILDLIST)
      (COND (*RLCADTREE2GMLXML (ATREE_2GML_NODE_XML TT NUMBER))
            (T (ATREE_2GML_NODE TT NUMBER)))
      (SETQ CHILDLIST (ATREE_CHILDL TT))
      (PROG (CHILD)
        (SETQ CHILD CHILDLIST)
       LAB
        (COND ((NULL CHILD) (RETURN NIL)))
        ((LAMBDA (CHILD)
           (SETQ NUMBER (ATREE_2GML_NODES CHILD (PLUS NUMBER 1))))
         (CAR CHILD))
        (SETQ CHILD (CDR CHILD))
        (GO LAB))
      (RETURN NUMBER))) 
(PUT 'ATREE_2GML_NODE_XML 'NUMBER-OF-ARGS 2) 
(DE ATREE_2GML_NODE_XML (TT NUMBER)
    (PROG (NAT C TV COLOR)
      (SETQ NAT *NAT)
      (OFF1 'NAT)
      (SETQ C (ATREE_ROOTCELL TT))
      (SETQ TV (ACELL_GETTV C))
      (SETQ COLOR
              (COND ((EQ TV 'TRUE) "#00FF00") ((EQ TV 'FALSE) "#FF0000")
                    (T "#C0C0C0")))
      (IOTO_PRIN2T "node [")
      (IOTO_PRIN2T (LIST "id " NUMBER))
      (IOTO_PRIN2T "label \"")
      (IOTO_PRIN2T "<node>")
      (IOTO_PRIN2T (LIST "<tv>" TV "</tv>"))
      (IOTO_PRIN2T (LIST "<idx>" (ACELL_GETIDX C) "</idx>"))
      (IOTO_PRIN2T "<tp>")
      (PROG (ANU)
        (SETQ ANU (REVERSE (ACELL_GETSP C)))
       LAB
        (COND ((NULL ANU) (RETURN NIL)))
        ((LAMBDA (ANU)
           (PROGN
            (IOTO_PRIN2T "<assignment>")
            (IOTO_PRIN2T (LIST "<var>" (AEX_MVAR (ANU_DP ANU)) "</var>"))
            (IOTO_PRIN2T "<poly>")
            (MATHPRINT (PREPSQ (AEX_EX (ANU_DP ANU))))
            (IOTO_PRIN2T "</poly>")
            (IOTO_PRIN2T "<lb>")
            (MATHPRINT (PREPSQ (IV_LB (ANU_IV ANU))))
            (IOTO_PRIN2T "</lb>")
            (IOTO_PRIN2T "<rb>")
            (MATHPRINT (PREPSQ (IV_RB (ANU_IV ANU))))
            (IOTO_PRIN2T "</rb>")
            (IOTO_PRIN2T "<floatapp>")
            (MATHPRINT (ANU_EVALFR ANU))
            (IOTO_PRIN2T "</floatapp>")
            (IOTO_PRIN2T "</assignment>")))
         (CAR ANU))
        (SETQ ANU (CDR ANU))
        (GO LAB))
      (IOTO_PRIN2T "</tp>")
      (IOTO_PRIN2T (LIST "<desc>" (ACELL_GETDESC C) "</desc>"))
      (IOTO_PRIN2T (LIST "<tl>" (ACELL_GETTL C) "</tl>"))
      (IOTO_PRIN2T "</node>")
      (IOTO_PRIN2T "\"")
      (IOTO_PRIN2T "graphics [")
      (IOTO_PRIN2T (LIST "fill \"" COLOR "\""))
      (COND ((EVENP (ACELL_GETIDX C)) (IOTO_PRIN2T "type \"ellipse\""))
            (T (IOTO_PRIN2T "type \"rectangle\"")))
      (IOTO_PRIN2T "]")
      (IOTO_PRIN2T "]")
      (COND (NAT (ON1 'NAT))))) 
(PUT 'ATREE_2GML_NODE 'NUMBER-OF-ARGS 2) 
(DE ATREE_2GML_NODE (TT NUMBER)
    (PROG (C TV ANUL VARL N COLOR TPL)
      (IOTO_PRIN2T "node [")
      (IOTO_PRIN2T (LIST "id " NUMBER))
      (SETQ C (ATREE_ROOTCELL TT))
      (IOTO_PRIN2T "label \"")
      (IOTO_PRIN2T (LIST "idx = " (ACELL_GETIDX C)))
      (SETQ ANUL
              (PROG (ANU FORALL-RESULT FORALL-ENDPTR)
                (SETQ ANU (REVERSE (ACELL_GETSP C)))
                (COND ((NULL ANU) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ANU) (ANU_EVALFR ANU)) (CAR ANU))
                                 NIL)))
               LOOPLABEL
                (SETQ ANU (CDR ANU))
                (COND ((NULL ANU) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ANU) (ANU_EVALFR ANU)) (CAR ANU)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARL
              (PROG (ANU FORALL-RESULT FORALL-ENDPTR)
                (SETQ ANU (REVERSE (ACELL_GETSP C)))
                (COND ((NULL ANU) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ANU) (AEX_MVAR (ANU_DP ANU)))
                                  (CAR ANU))
                                 NIL)))
               LOOPLABEL
                (SETQ ANU (CDR ANU))
                (COND ((NULL ANU) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ANU) (AEX_MVAR (ANU_DP ANU))) (CAR ANU))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (MATHPRINT (LIST 'EQUAL (CONS 'LIST VARL) (CONS 'LIST ANUL)))
      (MATHPRINT (LIST 'EQUAL 'TL (CONS 'LIST (ACELL_GETTL C))))
      (IOTO_PRIN2T "\"")
      (SETQ TV (ACELL_GETTV C))
      (SETQ COLOR
              (COND ((EQ TV 'TRUE) "#00FF00") ((EQ TV 'FALSE) "#FF0000")
                    (T "#C0C0C0")))
      (IOTO_PRIN2T "graphics [")
      (IOTO_PRIN2T (LIST "fill \"" COLOR "\""))
      (COND ((EVENP (ACELL_GETIDX C)) (IOTO_PRIN2T "type \"ellipse\""))
            (T (IOTO_PRIN2T "type \"rectangle\"")))
      (IOTO_PRIN2T "]")
      (IOTO_PRIN2T "]"))) 
(PUT 'ATREE_2GML_EDGES 'NUMBER-OF-ARGS 2) 
(DE ATREE_2GML_EDGES (TT NUMBER)
    (PROG (CHILDLIST MYNUMBER)
      (SETQ MYNUMBER NUMBER)
      (SETQ CHILDLIST (ATREE_CHILDL TT))
      (PROG (CHILD)
        (SETQ CHILD CHILDLIST)
       LAB
        (COND ((NULL CHILD) (RETURN NIL)))
        ((LAMBDA (CHILD)
           (PROGN
            (ATREE_2GML_EDGE MYNUMBER (PLUS NUMBER 1))
            (SETQ NUMBER (ATREE_2GML_EDGES CHILD (PLUS NUMBER 1)))))
         (CAR CHILD))
        (SETQ CHILD (CDR CHILD))
        (GO LAB))
      (RETURN NUMBER))) 
(PUT 'ATREE_2GML_EDGE 'NUMBER-OF-ARGS 2) 
(DE ATREE_2GML_EDGE (EFROM ETO)
    (PROG ()
      (IOTO_PRIN2T "edge [")
      (IOTO_PRIN2T (LIST "source " EFROM))
      (IOTO_PRIN2T (LIST "target " ETO))
      (IOTO_PRIN2T "]"))) 
(PUT 'ATREE_DELETEFALSELEAVES 'NUMBER-OF-ARGS 1) 
(DE ATREE_DELETEFALSELEAVES (TT)
    (PROG (W)
      (COND (*RLVERBOSE (IOTO_TPRIN2 "+ deleting false leaves ...")))
      (SETQ W (ATREE_DELETEFALSELEAVES1 TT 0 (ATREE_DEPTH TT)))
      (COND (*RLVERBOSE (IOTO_PRIN2T " done")))
      (RETURN W))) 
(PUT 'ATREE_DELETEFALSELEAVES1 'NUMBER-OF-ARGS 3) 
(DE ATREE_DELETEFALSELEAVES1 (TT N DPTH)
    (PROG (R CL NTT W)
      (SETQ R (ATREE_ROOTCELL TT))
      (COND ((AND (EQN N DPTH) (EQ (ACELL_GETTV R) 'FALSE)) (RETURN NIL)))
      (SETQ CL
              (PROG (C FORALL-RESULT FORALL-ENDPTR)
                (SETQ C (ATREE_CHILDL TT))
               STARTOVER
                (COND ((NULL C) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (C)
                           (PROGN
                            (SETQ W
                                    (ATREE_DELETEFALSELEAVES1 C (PLUS N 1)
                                     DPTH))
                            (COND (W (LIST W)))))
                         (CAR C)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ C (CDR C))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL C) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (C)
                           (PROGN
                            (SETQ W
                                    (ATREE_DELETEFALSELEAVES1 C (PLUS N 1)
                                     DPTH))
                            (COND (W (LIST W)))))
                         (CAR C)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ C (CDR C))
                (GO LOOPLABEL)))
      (SETQ NTT (ATREE_MK R))
      (ATREE_SETCHILDL NTT CL)
      (RETURN NTT))) 
(PUT 'ATREE_DEPTH 'NUMBER-OF-ARGS 1) 
(DE ATREE_DEPTH (TT) (ATREE_DEPTH1 TT 0)) 
(PUT 'ATREE_DEPTH1 'NUMBER-OF-ARGS 2) 
(DE ATREE_DEPTH1 (TT N)
    (PROG (D CL)
      (SETQ D 0)
      (SETQ CL (ATREE_CHILDL TT))
      (COND ((NULL CL) (RETURN N)))
      (PROG (C)
        (SETQ C CL)
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C) (SETQ D (MAX2 D (ATREE_DEPTH1 C (PLUS N 1))))) (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (RETURN D))) 
(PUT 'OFSF_SOLUTIONFORMULA_OLD 'NUMBER-OF-ARGS 1) 
(DE OFSF_SOLUTIONFORMULA_OLD (CD)
    (PROG (FFL DD K DDK FFK PHIPRIME CELLSTOGO)
      (COND
       (*RLVERBOSE (IOTO_TPRIN2T "+++ Solution Formula Construction Phase")))
      (SETQ FFL (CADDATA_FFL CD))
      (SETQ DD (CADDATA_DD CD))
      (SETQ K (CADDATA_K CD))
      (COND
       ((EQ K 0)
        (PROGN
         (CADDATA_PUTPHIPRIME CD (ACELL_GETTV (ATREE_ROOTCELL DD)))
         (RETURN NIL))))
      (SETQ DDK (ATREE_CHILDRENATLEVEL DD K))
      (SETQ FFK
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
               STARTOVER
                (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
                (SETQ FORALL-RESULT (NTH FFL I))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (PLUS2 I 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((MINUSP (DIFFERENCE K I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (NTH FFL I))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (PLUS2 I 1))
                (GO LOOPLABEL)))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (LIST "+ generating signatures for " (LENGTH FFK) " polynomials in "
               (LENGTH DDK) " cells"))))
      (SETQ CELLSTOGO (LENGTH DDK))
      (PROG (CELL)
        (SETQ CELL DDK)
       LAB
        (COND ((NULL CELL) (RETURN NIL)))
        ((LAMBDA (CELL)
           (PROGN
            (ACELL_PUTDESC CELL (OFSF_SIGNATURE FFK (ACELL_GETSP CELL)))
            (COND
             ((OFSF_CADVERBOSEP)
              (IOTO_PRIN2
               (LIST "[" CELLSTOGO
                     (COND ((EQ (ACELL_GETTV CELL) 'TRUE) " tt") (T " ff"))
                     "] "))))
            (SETQ CELLSTOGO (DIFFERENCE CELLSTOGO 1))))
         (CAR CELL))
        (SETQ CELL (CDR CELL))
        (GO LAB))
      (SETQ PHIPRIME (OFSF_SFCHONG FFK DDK))
      (COND
       ((EQ PHIPRIME 'SSFCFAILED)
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T "+ Solution formula construction ssfc failed.")))
         (RETURN NIL)))
       (T
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T "+ Solution formula construction ssfc successful.")))
         (COND (*RLCADRAWFORMULA (CADDATA_PUTPHIPRIME CD PHIPRIME))
               (T (CADDATA_PUTPHIPRIME CD (RL_DNF PHIPRIME)))))))
      (RETURN NIL))) 
(PUT 'OFSF_SFCHONG 'NUMBER-OF-ARGS 2) 
(DE OFSF_SFCHONG (FFK DDK)
    (PROG (WWT WWF WWC)
      (SETQ WWT (OFSF_SIGNATURESBYTV DDK 'TRUE))
      (SETQ WWF (OFSF_SIGNATURESBYTV DDK 'FALSE))
      (SETQ WWC (INTERSECTION WWT WWF))
      (COND
       ((NULL WWC)
        (RETURN
         ((LAMBDA (G293)
            (COND ((AND G293 (CDR G293)) (CONS 'OR G293))
                  ((NULL G293) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G293))))
          (PROG (SIG FORALL-RESULT FORALL-ENDPTR)
            (SETQ SIG WWT)
            (COND ((NULL SIG) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (SIG) (OFSF_SIGBASEDFO FFK SIG))
                              (CAR SIG))
                             NIL)))
           LOOPLABEL
            (SETQ SIG (CDR SIG))
            (COND ((NULL SIG) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (SIG) (OFSF_SIGBASEDFO FFK SIG)) (CAR SIG))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (RETURN 'SSFCFAILED))) 
(PUT 'OFSF_SOLUTIONFORMULA 'NUMBER-OF-ARGS 1) 
(DE OFSF_SOLUTIONFORMULA (CD)
    (PROG (DD YY W YYI FFL K FFLK PHIPRIME)
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T "+++ Simple Solution Formula Construction Phase")))
      (SETQ DD (CADDATA_DD CD))
      (COND
       ((MEMQ (ACELL_GETTV (ATREE_ROOTCELL DD)) '(TRUE FALSE))
        (PROGN
         (COND
          ((AND *RLVERBOSE *RLCADANS)
           (IOTO_PRIN2T
            (LIST "+ ANSWERS (for decision problem): "
                  (CDR (ATSOC 'ANSWERS (ACELL_GETTL (ATREE_ROOTCELL DD))))))))
         (CADDATA_PUTPHIPRIME CD (ACELL_GETTV (ATREE_ROOTCELL DD)))
         (RETURN NIL))))
      (SETQ YY (ATREE_CELLTVL DD))
      (COND
       ((AND *RLVERBOSE *RLCADANS)
        (PROGN
         (SETQ W
                 (PROG (CELL FORALL-RESULT FORALL-ENDPTR)
                   (SETQ CELL YY)
                  STARTOVER
                   (COND ((NULL CELL) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (CELL)
                              (COND
                               ((EQ (ACELL_GETTV CELL) 'TRUE)
                                (LIST (ATSOC 'ANSWERS (ACELL_GETTL CELL))))))
                            (CAR CELL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ CELL (CDR CELL))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL CELL) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (CELL)
                              (COND
                               ((EQ (ACELL_GETTV CELL) 'TRUE)
                                (LIST (ATSOC 'ANSWERS (ACELL_GETTL CELL))))))
                            (CAR CELL)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ CELL (CDR CELL))
                   (GO LOOPLABEL)))
         (PROG (C)
           (SETQ C (LTO_LIST2SET W))
          LAB
           (COND ((NULL C) (RETURN NIL)))
           ((LAMBDA (C) (IOTO_PRIN2T (LIST "+ ANSWERS: " C))) (CAR C))
           (SETQ C (CDR C))
           (GO LAB)))))
      (SETQ YYI
              (LTO_LIST2SET
               (PROG (CELL FORALL-RESULT FORALL-ENDPTR)
                 (SETQ CELL YY)
                 (COND ((NULL CELL) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (CELL) (LENGTH (ACELL_GETSP CELL)))
                                   (CAR CELL))
                                  NIL)))
                LOOPLABEL
                 (SETQ CELL (CDR CELL))
                 (COND ((NULL CELL) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (CELL) (LENGTH (ACELL_GETSP CELL)))
                           (CAR CELL))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (COND
       (*RLVERBOSE (IOTO_PRIN2T (LIST "+ levels to be considered: " YYI))))
      (SETQ FFL (CADDATA_FFL CD))
      (SETQ FFLK
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I YYI)
               STARTOVER
                (COND ((NULL I) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (I) (APPEND (NTH FFL I) NIL)) (CAR I)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (CDR I))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL I) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (I) (APPEND (NTH FFL I) NIL)) (CAR I)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (CDR I))
                (GO LOOPLABEL)))
      (SETQ K (CADDATA_K CD))
      (SETQ PHIPRIME (OFSF_SOLUTIONFORMULA1 DD FFLK YY K))
      (COND
       ((EQ PHIPRIME 'SSFCFAILED)
        (PROGN
         (COND
          (*RLVERBOSE
           (IOTO_TPRIN2T
            "+ SSFC failed, trying all possible projection factors.")))
         (SETQ YYI
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I 1)
                   (COND ((MINUSP (DIFFERENCE K I)) (RETURN NIL)))
                   (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS I NIL)))
                  LOOPLABEL
                   (SETQ I (PLUS2 I 1))
                   (COND ((MINUSP (DIFFERENCE K I)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR (CONS I NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (COND
          (*RLVERBOSE (IOTO_PRIN2T (LIST "+ Levels to be considered: " YYI))))
         (SETQ FFLK
                 (PROG (I FORALL-RESULT FORALL-ENDPTR)
                   (SETQ I YYI)
                  STARTOVER
                   (COND ((NULL I) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (I) (APPEND (NTH FFL I) NIL)) (CAR I)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ I (CDR I))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL I) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (I) (APPEND (NTH FFL I) NIL)) (CAR I)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ I (CDR I))
                   (GO LOOPLABEL)))
         (SETQ PHIPRIME (OFSF_SOLUTIONFORMULA1 DD FFLK YY K)))))
      (COND
       ((EQ PHIPRIME 'SSFCFAILED)
        (PROGN
         (COND (*RLVERBOSE (IOTO_TPRIN2T "+ SSFC failed.")))
         (RETURN NIL))))
      (COND (*RLVERBOSE (IOTO_TPRIN2T "+ SSFC succeded.")))
      (COND (*RLCADRAWFORMULA (CADDATA_PUTPHIPRIME CD PHIPRIME))
            (T (CADDATA_PUTPHIPRIME CD (RL_DNF PHIPRIME))))
      (RETURN NIL))) 
(PUT 'OFSF_SOLUTIONFORMULA1 'NUMBER-OF-ARGS 4) 
(DE OFSF_SOLUTIONFORMULA1 (DD FFK YY K)
    (PROG (CELLSTOGO)
      (SETQ CELLSTOGO 0)
      (COND
       (*RLVERBOSE
        (PROGN
         (IOTO_TPRIN2T
          (LIST "+ Generating signatures for " (LENGTH FFK) " polynomials and "
                (LENGTH YY) " cells."))
         (IOTO_TPRIN2T
          (LIST "+ Number of cells on level " K " is "
                (LENGTH (ATREE_CHILDRENATLEVEL DD K)) "."))
         (SETQ CELLSTOGO (LENGTH YY))
         NIL)))
      (PROG (CELL)
        (SETQ CELL YY)
       LAB
        (COND ((NULL CELL) (RETURN NIL)))
        ((LAMBDA (CELL)
           (PROGN
            (ACELL_PUTDESC CELL (OFSF_SIGNATURE4 FFK (ACELL_GETSP CELL)))
            (COND
             ((OFSF_CADVERBOSEP)
              (PROGN
               (IOTO_PRIN2
                (LIST "[" CELLSTOGO " " "sig" (ACELL_GETDESC CELL) " "
                      (COND ((EQ (ACELL_GETTV CELL) 'TRUE) "true") (T "false"))
                      "] "))
               (SETQ CELLSTOGO (DIFFERENCE CELLSTOGO 1)))))))
         (CAR CELL))
        (SETQ CELL (CDR CELL))
        (GO LAB))
      (RETURN (OFSF_SSFC2 FFK YY)))) 
(PUT 'OFSF_SSFC2 'NUMBER-OF-ARGS 2) 
(DE OFSF_SSFC2 (FFK YY)
    (PROG (WWT WWF WWC)
      (SETQ WWT (OFSF_SIGNATURESBYTV YY 'TRUE))
      (SETQ WWF (OFSF_SIGNATURESBYTV YY 'FALSE))
      (SETQ WWC (OFSF_COMPSIG WWT WWF))
      (COND
       ((NULL WWC)
        (RETURN
         ((LAMBDA (G295)
            (COND ((AND G295 (CDR G295)) (CONS 'OR G295))
                  ((NULL G295) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G295))))
          (PROG (SIG FORALL-RESULT FORALL-ENDPTR)
            (SETQ SIG WWT)
            (COND ((NULL SIG) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (SIG) (OFSF_SIGBASEDFO FFK SIG))
                              (CAR SIG))
                             NIL)))
           LOOPLABEL
            (SETQ SIG (CDR SIG))
            (COND ((NULL SIG) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (SIG) (OFSF_SIGBASEDFO FFK SIG)) (CAR SIG))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (COND
       ((OFSF_CADVERBOSEP)
        (IOTO_PRIN2T
         (LIST "+ SSFC failed because of these compatible signatures: " WWC))))
      (RETURN 'SSFCFAILED))) 
(PUT 'OFSF_COMPSIG 'NUMBER-OF-ARGS 2) 
(DE OFSF_COMPSIG (WW1 WW2)
    (PROG (WW2COPY RETVALUE W1 W2)
      (COND ((NULL WW1) (RETURN NIL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND WW1 (NOT RETVALUE))) (RETURN NIL)))
        (PROGN
         (SETQ W1 (PROG1 (CAR WW1) (SETQ WW1 (CDR WW1))))
         (SETQ WW2COPY WW2)
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND WW2COPY (NOT RETVALUE))) (RETURN NIL)))
           (PROGN
            (SETQ W2 (PROG1 (CAR WW2COPY) (SETQ WW2COPY (CDR WW2COPY))))
            (COND ((OFSF_COMPSIG1 W1 W2) (SETQ RETVALUE (LIST W1 W2)))))
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN RETVALUE))) 
(PUT 'OFSF_COMPSIG1 'NUMBER-OF-ARGS 2) 
(DE OFSF_COMPSIG1 (W1 W2)
    (PROG (C S1 S2)
      (COND ((NULL W1) (RETURN T)))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND W1 C)) (RETURN NIL)))
        (PROGN
         (SETQ S1 (PROG1 (CAR W1) (SETQ W1 (CDR W1))))
         (SETQ S2 (PROG1 (CAR W2) (SETQ W2 (CDR W2))))
         (COND
          ((AND (NEQ S1 S2) (NOT (EQUAL S1 "?")) (NOT (EQUAL S2 "?")))
           (SETQ C NIL))))
        (GO WHILELABEL))
      (RETURN C))) 
(PUT 'OFSF_SIGBASEDFO 'NUMBER-OF-ARGS 2) 
(DE OFSF_SIGBASEDFO (FFK SIGL)
    (PROG (SIG FO)
      (PROG (F)
        (SETQ F FFK)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ SIG (PROG1 (CAR SIGL) (SETQ SIGL (CDR SIGL))))
            (COND
             ((NOT (EQUAL SIG "?"))
              (PROG (W1)
                (SETQ W1 (OFSF_SIGNOFPOLYFO F SIG))
                (SETQ FO (CONS W1 FO))
                (RETURN W1))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN
       (COND ((AND FO (CDR FO)) (CONS 'AND FO))
             ((NULL FO) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR FO)))))) 
(PUT 'OFSF_SIGNOFPOLYFO 'NUMBER-OF-ARGS 2) 
(DE OFSF_SIGNOFPOLYFO (F S)
    (COND ((EQN S (MINUS 1)) (LIST 'LESSP F NIL))
          ((EQN S 0) (LIST 'EQUAL F NIL)) ((EQN S 1) (LIST 'GREATERP F NIL)))) 
(PUT 'OFSF_EVALQFF 'NUMBER-OF-ARGS 3) 
(DE OFSF_EVALQFF (F SP VARL)
    (COND (*RLCADFASTEVAL (OFSF_EVALQFF-FAST F SP VARL))
          (T
           (CL_SIMPL (CL_APPLY2ATS1 F (FUNCTION OFSF_SUBSIGNAT) (LIST SP VARL))
                     NIL (MINUS 1))))) 
(PUT 'OFSF_EVALSIGNF 'NUMBER-OF-ARGS 3) 
(DE OFSF_EVALSIGNF (F SP VARL)
    (CAR (SIMP (AEX_SGN (OFSF_SUBSP (AEX_FROMSF F) SP VARL))))) 
(PUT 'OFSF_TRIALEVALSGNF 'NUMBER-OF-ARGS 2) 
(DE OFSF_TRIALEVALSGNF (F SP)
    (PROGN
     (SETQ F (OFSF_SUBSP* (AEX_FROMSF F) SP))
     (COND ((AEX_SIMPLENUMBERP F) (CAR (SIMP (AEX_SGN F))))
           (T (CAR (AEX_EX F)))))) 
(PUT 'OFSF_SGNF4 'NUMBER-OF-ARGS 2) 
(DE OFSF_SGNF4 (F SP)
    (PROGN
     (SETQ F (OFSF_SUBSP* (AEX_FROMSF F) SP))
     (COND ((AEX_SIMPLENUMBERP F) (AEX_SGN F)) (T "?")))) 
(PUT 'OFSF_SUBSIGNAT 'NUMBER-OF-ARGS 3) 
(DE OFSF_SUBSIGNAT (AT SP VARL)
    (LIST (CAR AT) (OFSF_EVALSIGNF (CADR AT) SP VARL) NIL)) 
(PUT 'OFSF_EVALQFF-FAST 'NUMBER-OF-ARGS 3) 
(DE OFSF_EVALQFF-FAST (F SP VARL)
    (COND ((CL_ATFP F) (OFSF_SIMPLAT1 (OFSF_SUBSIGNAT F SP VARL) NIL))
          (T (OFSF_EVALQFF-FAST1 F SP VARL)))) 
(PUT 'OFSF_EVALQFF-FAST1 'NUMBER-OF-ARGS 3) 
(DE OFSF_EVALQFF-FAST1 (F SP VARL)
    (COND ((OR (EQ F 'TRUE) (EQ F 'FALSE)) F)
          (T
           (OFSF_EVALQFF-GAND (COND ((ATOM F) F) (T (CAR F))) (CDR F) SP
            VARL)))) 
(PUT 'OFSF_EVALQFF-GAND 'NUMBER-OF-ARGS 4) 
(DE OFSF_EVALQFF-GAND (GAND ARGL SP VARL)
    (PROG (GFALSE ARG CARGL C)
      (SETQ GFALSE (CL_CFLIP 'FALSE (EQ GAND 'AND)))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ARGL C)) (RETURN NIL)))
        (PROGN
         (SETQ ARG (CAR ARGL))
         (SETQ ARGL (CDR ARGL))
         (COND
          ((CL_ATFP ARG)
           (COND
            ((EQ (OFSF_SIMPLAT1 (OFSF_SUBSIGNAT ARG SP VARL) NIL) GFALSE)
             (SETQ C NIL))))
          (T (SETQ CARGL (CONS ARG CARGL)))))
        (GO WHILELABEL))
      (COND ((NOT C) (RETURN GFALSE)))
      (SETQ C T)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND CARGL C)) (RETURN NIL)))
        (PROGN
         (SETQ ARG (CAR CARGL))
         (SETQ CARGL (CDR CARGL))
         (COND ((EQ (OFSF_EVALQFF-FAST1 ARG SP VARL) GFALSE) (SETQ C NIL))))
        (GO WHILELABEL))
      (COND ((NOT C) (RETURN GFALSE)))
      (RETURN (CL_FLIP GFALSE)))) 
(PUT 'OFSF_SIGNATURE 'NUMBER-OF-ARGS 2) 
(DE OFSF_SIGNATURE (FK SP)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F FK)
      (COND ((NULL F) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (F) (AEX_SGN (OFSF_SUBSP* (AEX_FROMSF F) SP)))
                        (CAR F))
                       NIL)))
     LOOPLABEL
      (SETQ F (CDR F))
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (F) (AEX_SGN (OFSF_SUBSP* (AEX_FROMSF F) SP))) (CAR F))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OFSF_SIGNATURE4 'NUMBER-OF-ARGS 2) 
(DE OFSF_SIGNATURE4 (FK SP)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F FK)
      (COND ((NULL F) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (F) (OFSF_SGNF4 F SP)) (CAR F)) NIL)))
     LOOPLABEL
      (SETQ F (CDR F))
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (F) (OFSF_SGNF4 F SP)) (CAR F)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'OFSF_SIGNATURESBYTV 'NUMBER-OF-ARGS 2) 
(DE OFSF_SIGNATURESBYTV (DDK TV)
    (PROG (C FORALL-RESULT FORALL-ENDPTR)
      (SETQ C DDK)
     STARTOVER
      (COND ((NULL C) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (C)
                 (COND ((EQ (ACELL_GETTV C) TV) (LIST (ACELL_GETDESC C)))))
               (CAR C)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ C (CDR C))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL C) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (C)
                 (COND ((EQ (ACELL_GETTV C) TV) (LIST (ACELL_GETDESC C)))))
               (CAR C)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ C (CDR C))
      (GO LOOPLABEL))) 
(ENDMODULE) 