(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFVSBLOCK)) 
(REVISION 'OFSFVSBLOCK
          "$Id: ofsfvsblock.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFVSBLOCK "(c) 2015-2017 M. Kosta, T. Sturm") 
(FLUID '(RLVARSELLVL*)) 
(SETQ RLVARSELLVL* 3) 
(SWITCH (LIST 'OFSFVSBLOCKBTR)) 
(OFF1 'OFSFVSBLOCKBTR) 
(SWITCH (LIST 'OFSFVSQETREE2GML)) 
(OFF1 'OFSFVSQETREE2GML) 
(FLUID '(RLQETREEGMLFILE*)) 
(SETQ RLQETREEGMLFILE* "/tmp/qe-tree.gml") 
(FLUID '(VS_FNAL*)) 
(PUT 'VS_SETFNAL 'NUMBER-OF-ARGS 0) 
(DE VS_SETFNAL NIL
    (PROGN
     (VS_PATCHFNAL 'FN_PC-DECOMPOSITION 'VSDE_PC-DECOMPOSITION)
     (VS_PATCHFNAL 'FN_APPLYVSTS 'VSDS_APPLYVSTS))) 
(PUT 'VS_PATCHFNAL 'NUMBER-OF-ARGS 2) 
(DE VS_PATCHFNAL (KEY VALUE) (SETQ VS_FNAL* (LTO_ALPATCH KEY VALUE VS_FNAL*))) 
(PUT 'VS_APPLYFN 'NUMBER-OF-ARGS 2) 
(DE VS_APPLYFN (FN ARGL) (APPLY (CDR (ATSOC FN VS_FNAL*)) ARGL)) 
(PUT 'VSND_MK 'NUMBER-OF-ARGS 6) 
(DE VSND_MK (FLG VS VL F P TVL) (LIST 'VSND FLG VS VL F P TVL)) 
(PUT 'VSND_FLG 'NUMBER-OF-ARGS 1) 
(DE VSND_FLG (ND) (NTH ND 2)) 
(PUT 'VSND_VS 'NUMBER-OF-ARGS 1) 
(DE VSND_VS (ND) (NTH ND 3)) 
(PUT 'VSND_VL 'NUMBER-OF-ARGS 1) 
(DE VSND_VL (ND) (NTH ND 4)) 
(PUT 'VSND_F 'NUMBER-OF-ARGS 1) 
(DE VSND_F (ND) (NTH ND 5)) 
(PUT 'VSND_PARENT 'NUMBER-OF-ARGS 1) 
(DE VSND_PARENT (ND) (NTH ND 6)) 
(PUT 'VSND_TVL 'NUMBER-OF-ARGS 1) 
(DE VSND_TVL (ND) (NTH ND 7)) 
(PUT 'VSCO_MK 'NUMBER-OF-ARGS 0) 
(DE VSCO_MK NIL NIL) 
(PUT 'VSCO_NONEMPTYP 'NUMBER-OF-ARGS 1) 
(DE VSCO_NONEMPTYP (CO) CO) 
(PUT 'VSCO_INSERT 'NUMBER-OF-ARGS 2) 
(DE VSCO_INSERT (CO ND) (CONS ND CO)) 
(PUT 'VSCO_GET 'NUMBER-OF-ARGS 1) 
(DE VSCO_GET (CO) (PROGN (COND (NIL NIL)) CO)) 
(PUT 'VSCO_LENGTH 'NUMBER-OF-ARGS 1) 
(DE VSCO_LENGTH (CO) (LENGTH CO)) 
(PUT 'VSHT_MK 'NUMBER-OF-ARGS 0) 
(DE VSHT_MK NIL (LIST 'VSHT NIL)) 
(PUT 'VSHT_INSERT 'NUMBER-OF-ARGS 2) 
(DE VSHT_INSERT (HT F) (LIST 'VSHT (LTO_HINSERT F (NTH HT 2) 'VSHT_HFN))) 
(PUT 'VSHT_MEMBER 'NUMBER-OF-ARGS 2) 
(DE VSHT_MEMBER (HT F) (LTO_HMEMBER F (NTH HT 2) 'VSHT_HFN)) 
(PUT 'VSHT_HFN 'NUMBER-OF-ARGS 1) 
(DE VSHT_HFN (F) (LIST F)) 
(PUT 'VSCO_CDELETE 'NUMBER-OF-ARGS 2) 
(DE VSCO_CDELETE (CO ND)
    (COND ((NULL CO) NIL)
          ((OR (EQ ND (CAR CO)) (EQ ND (VSND_PARENT (CAR CO))))
           (VSCO_CDELETE (CDR CO) ND))
          (T (CONS (CAR CO) (VSCO_CDELETE (CDR CO) ND))))) 
(PUT 'VSDB_NEW 'NUMBER-OF-ARGS 0) 
(DE VSDB_NEW NIL
    (PROG (DB)
      (SETQ DB (MKVECT 10))
      (PUTV DB 0 'VSDB)
      (PUTV DB 1 'UNDEFINED)
      (PUTV DB 2 'UNDEFINED)
      (PUTV DB 3 'UNDEFINED)
      (PUTV DB 4 'UNDEFINED)
      (PUTV DB 5 'UNDEFINED)
      (PUTV DB 6 'UNDEFINED)
      (PUTV DB 7 'UNDEFINED)
      (PUTV DB 8 'UNDEFINED)
      (PUTV DB 9 'UNDEFINED)
      (PUTV DB 10 'UNDEFINED)
      (RETURN DB))) 
(PUT 'VSDB_VL 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_VL 'DEFINED-ON-LINE '189) 
(PUT 'VSDB_VL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_VL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_VL (DB) (GETV DB 1)) 
(PUT 'VSDB_F 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_F 'DEFINED-ON-LINE '190) 
(PUT 'VSDB_F 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_F (DB) (GETV DB 2)) 
(PUT 'VSDB_THEO 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_THEO 'DEFINED-ON-LINE '191) 
(PUT 'VSDB_THEO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_THEO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_THEO (DB) (GETV DB 3)) 
(PUT 'VSDB_BVL 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_BVL 'DEFINED-ON-LINE '192) 
(PUT 'VSDB_BVL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_BVL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_BVL (DB) (GETV DB 4)) 
(PUT 'VSDB_ANS 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_ANS 'DEFINED-ON-LINE '193) 
(PUT 'VSDB_ANS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_ANS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_ANS (DB) (GETV DB 5)) 
(PUT 'VSDB_WC 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_WC 'DEFINED-ON-LINE '194) 
(PUT 'VSDB_WC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_WC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_WC (DB) (GETV DB 6)) 
(PUT 'VSDB_SC 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_SC 'DEFINED-ON-LINE '195) 
(PUT 'VSDB_SC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_SC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_SC (DB) (GETV DB 7)) 
(PUT 'VSDB_FC 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_FC 'DEFINED-ON-LINE '196) 
(PUT 'VSDB_FC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_FC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_FC (DB) (GETV DB 8)) 
(PUT 'VSDB_HT 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_HT 'DEFINED-ON-LINE '197) 
(PUT 'VSDB_HT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_HT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_HT (DB) (GETV DB 9)) 
(PUT 'VSDB_CURTHEO 'NUMBER-OF-ARGS 1) 
(PUT 'VSDB_CURTHEO 'DEFINED-ON-LINE '198) 
(PUT 'VSDB_CURTHEO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_CURTHEO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VSDB_CURTHEO (DB) (GETV DB 10)) 
(PUT 'VSDB_PUTVL 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTVL 'DEFINED-ON-LINE '200) 
(PUT 'VSDB_PUTVL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTVL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTVL (DB VL) (PUTV DB 1 VL)) 
(PUT 'VSDB_PUTF 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTF 'DEFINED-ON-LINE '201) 
(PUT 'VSDB_PUTF 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTF (DB F) (PUTV DB 2 F)) 
(PUT 'VSDB_PUTTHEO 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTTHEO 'DEFINED-ON-LINE '202) 
(PUT 'VSDB_PUTTHEO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTTHEO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTTHEO (DB THEO) (PUTV DB 3 THEO)) 
(PUT 'VSDB_PUTBVL 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTBVL 'DEFINED-ON-LINE '203) 
(PUT 'VSDB_PUTBVL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTBVL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTBVL (DB BVL) (PUTV DB 4 BVL)) 
(PUT 'VSDB_PUTANS 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTANS 'DEFINED-ON-LINE '204) 
(PUT 'VSDB_PUTANS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTANS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTANS (DB ANS) (PUTV DB 5 ANS)) 
(PUT 'VSDB_PUTWC 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTWC 'DEFINED-ON-LINE '205) 
(PUT 'VSDB_PUTWC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTWC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTWC (DB WC) (PUTV DB 6 WC)) 
(PUT 'VSDB_PUTSC 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTSC 'DEFINED-ON-LINE '206) 
(PUT 'VSDB_PUTSC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTSC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTSC (DB SC) (PUTV DB 7 SC)) 
(PUT 'VSDB_PUTFC 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTFC 'DEFINED-ON-LINE '207) 
(PUT 'VSDB_PUTFC 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTFC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTFC (DB FC) (PUTV DB 8 FC)) 
(PUT 'VSDB_PUTHT 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTHT 'DEFINED-ON-LINE '208) 
(PUT 'VSDB_PUTHT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTHT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTHT (DB HT) (PUTV DB 9 HT)) 
(PUT 'VSDB_PUTCURTHEO 'NUMBER-OF-ARGS 2) 
(PUT 'VSDB_PUTCURTHEO 'DEFINED-ON-LINE '209) 
(PUT 'VSDB_PUTCURTHEO 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFVSBLOCK.RED) 
(PUT 'VSDB_PUTCURTHEO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VSDB_PUTCURTHEO (DB THEO) (PUTV DB 10 THEO)) 
(PUT 'VSDB_MK 'NUMBER-OF-ARGS 5) 
(DE VSDB_MK (VL F THEO BVL ANS)
    (PROG (DB)
      (SETQ DB (VSDB_NEW))
      (VSDB_PUTVL DB VL)
      (VSDB_PUTF DB F)
      (VSDB_PUTTHEO DB THEO)
      (VSDB_PUTBVL DB BVL)
      (VSDB_PUTANS DB ANS)
      (VSDB_PUTWC DB (VSCO_MK))
      (VSDB_PUTSC DB (VSCO_MK))
      (VSDB_PUTFC DB (VSCO_MK))
      (VSDB_PUTHT DB (VSHT_MK))
      (VSDB_PUTCURTHEO DB THEO)
      (VSDB_WCINSERT DB (VSND_MK NIL NIL VL F NIL NIL))
      (RETURN DB))) 
(PUT 'VSDB_TODOP 'NUMBER-OF-ARGS 1) 
(DE VSDB_TODOP (DB) (VSCO_NONEMPTYP (VSDB_WC DB))) 
(PUT 'VSDB_WCGET 'NUMBER-OF-ARGS 1) 
(DE VSDB_WCGET (DB)
    (PROG (C)
      (COND (NIL NIL))
      (SETQ C (VSCO_GET (VSDB_WC DB)))
      (VSDB_PUTWC DB (CDR C))
      (RETURN (CAR C)))) 
(PUT 'VSDB_SCGET 'NUMBER-OF-ARGS 1) 
(DE VSDB_SCGET (DB)
    (PROG (C)
      (COND (NIL NIL))
      (SETQ C (VSCO_GET (VSDB_SC DB)))
      (VSDB_PUTSC DB (CDR C))
      (RETURN (CAR C)))) 
(PUT 'VSDB_DROPALL 'NUMBER-OF-ARGS 1) 
(DE VSDB_DROPALL (DB)
    (PROGN
     (VSDB_PUTWC DB (VSCO_MK))
     (VSDB_PUTSC DB (VSCO_MK))
     (VSDB_PUTFC DB (VSCO_MK)))) 
(PUT 'VSDB_HTMEMBER 'NUMBER-OF-ARGS 2) 
(DE VSDB_HTMEMBER (DB F) (VSHT_MEMBER (VSDB_HT DB) F)) 
(PUT 'VSDB_WCINSERT 'NUMBER-OF-ARGS 2) 
(DE VSDB_WCINSERT (DB ND) (VSDB_PUTWC DB (VSCO_INSERT (VSDB_WC DB) ND))) 
(PUT 'VSDB_SCINSERT 'NUMBER-OF-ARGS 2) 
(DE VSDB_SCINSERT (DB ND) (VSDB_PUTSC DB (VSCO_INSERT (VSDB_SC DB) ND))) 
(PUT 'VSDB_FCINSERT 'NUMBER-OF-ARGS 2) 
(DE VSDB_FCINSERT (DB ND) (VSDB_PUTFC DB (VSCO_INSERT (VSDB_FC DB) ND))) 
(PUT 'VSDB_HTINSERT 'NUMBER-OF-ARGS 2) 
(DE VSDB_HTINSERT (DB F) (VSDB_PUTHT DB (VSHT_INSERT (VSDB_HT DB) F))) 
(PUT 'VSDB_CDELETE 'NUMBER-OF-ARGS 2) 
(DE VSDB_CDELETE (DB ND)
    (PROGN
     (VSDB_PUTWC DB (VSCO_CDELETE (VSDB_WC DB) ND))
     (VSDB_PUTSC DB (VSCO_CDELETE (VSDB_SC DB) ND))
     (VSDB_PUTFC DB (VSCO_CDELETE (VSDB_FC DB) ND)))) 
(PUT 'VS_BLOCK 'NUMBER-OF-ARGS 5) 
(DE VS_BLOCK (F VL THEO ANS BVL)
    (PROG (DB RF RVL ANSAL)
      (VS_SETFNAL)
      (SETQ DB (VSDB_MK VL F THEO BVL ANS))
      (COND (*OFSFVSBLOCKBTR (VS_BLOCKMAINLOOP-BTR DB))
            (T (VS_BLOCKMAINLOOP DB)))
      (PROG (G544)
        (SETQ G544 (VSDB_COLLECTRESULT DB))
        (SETQ RF (CAR G544))
        (SETQ RVL (CDR G544))
        (RETURN G544))
      (COND (*OFSFVSQETREE2GML (VSDB_2GML DB RLQETREEGMLFILE*)))
      (COND
       ((VSDB_ANS DB)
        (PROGN
         (SETQ ANSAL (VSDB_COMPUTEANS DB))
         (PROG (PR)
           (SETQ PR ANSAL)
          LAB
           (COND ((NULL PR) (RETURN NIL)))
           ((LAMBDA (PR)
              (IOTO_PRIN2T
               (LIST (CAR PR) " = " (IOTO_SMAPRIN (ANU_EVALFR (CDR PR))))))
            (CAR PR))
           (SETQ PR (CDR PR))
           (GO LAB)))))
      (RETURN (LIST RVL (LIST (CONS RF NIL)) THEO)))) 
(PUT 'VS_BLOCKMAINLOOP-BTR 'NUMBER-OF-ARGS 1) 
(DE VS_BLOCKMAINLOOP-BTR (DB)
    (PROG (ND)
      (PROG ()
       WHILELABEL
        (COND ((NOT (VSDB_TODOP DB)) (RETURN NIL)))
        (PROGN
         (SETQ ND (VSDB_WCGET DB))
         (COND ((VSND_FLG ND) (VSDB_APPLYVS DB ND))
               (T
                (PROGN
                 (COND ((EQ (VSND_F ND) 'TRUE) (VSDB_DROPALL DB)))
                 (COND ((NULL (VSND_VL ND)) (VSDB_SCINSERT DB ND))
                       (T
                        (PROGN
                         (COND (*RLVERBOSE (VSND_PRINTINFO ND)))
                         (VSDB_EXPANDNODE-BTR DB ND))))))))
        (GO WHILELABEL)))) 
(PUT 'VSDB_EXPANDNODE-BTR 'NUMBER-OF-ARGS 2) 
(DE VSDB_EXPANDNODE-BTR (DB ND)
    (PROG (VL PN DE V F NVL)
      (COND (NIL NIL))
      (COND (NIL NIL))
      (COND ((VSDB_TRYEXPANDNO DB ND) (RETURN NIL)))
      (COND ((VSDB_TRYEXPANDDG DB ND) (RETURN NIL)))
      (SETQ VL (LTO_SETMINUS (VSND_VL ND) (VSND_TVL ND)))
      (COND
       ((NULL VL)
        (PROGN
         (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "+++++ BACK"))))
         (SETQ PN (VSND_PARENT ND))
         (COND
          ((NULL PN)
           (PROGN (VSDB_DROPALL DB) (VSDB_FCINSERT DB ND) (RETURN NIL))))
         (VSDB_CDELETE DB PN)
         (VSDB_WCINSERT DB
          (VSND_MK (VSND_FLG PN) (VSND_VS PN) (VSND_VL PN) (VSND_F PN)
           (VSND_PARENT PN) (CONS (VSVS_V (VSND_VS ND)) (VSND_TVL PN))))
         (RETURN NIL))))
      (SETQ DE (VSDB_SELECTVAR-BTR DB ND))
      (SETQ V (VSDE_V DE))
      (COND
       ((NULL (VSDE_TPL DE))
        (PROGN
         (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "+++++ FAIL " V))))
         (VSDB_WCINSERT DB
          (VSND_MK (VSND_FLG ND) (VSND_VS ND) (VSND_VL ND) (VSND_F ND)
           (VSND_PARENT ND) (CONS V (VSND_TVL ND))))
         (RETURN NIL))))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (LIST "+++++ SUCC " (VSDE_V DE) " (elimset size = "
               (LENGTH (VSDE_TPL DE)) ")"))))
      (SETQ F (VSDE_F DE))
      (SETQ NVL (LTO_DELQ V (VSND_VL ND)))
      (PROG (TP)
        (SETQ TP (VSDE_TPL DE))
       LAB
        (COND ((NULL TP) (RETURN NIL)))
        ((LAMBDA (TP)
           (VSDB_WCINSERT DB (VSND_MK T (VSTS_MK V TP) NVL F ND NIL)))
         (CAR TP))
        (SETQ TP (CDR TP))
        (GO LAB)))) 
(PUT 'VSDB_SELECTVAR-BTR 'NUMBER-OF-ARGS 2) 
(DE VSDB_SELECTVAR-BTR (DB ND)
    (PROG (VL F THEO BVL SLTD V OO DE DEL)
      (SETQ VL (LTO_SETMINUS (VSND_VL ND) (VSND_TVL ND)))
      (SETQ F (VSND_F ND))
      (SETQ THEO (VSDB_CURTHEO DB))
      (SETQ BVL (VSDB_BVL DB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND VL (NOT SLTD))) (RETURN NIL)))
        (PROGN
         (SETQ V (PROG1 (CAR VL) (SETQ VL (CDR VL))))
         (SETQ OO (UPDKORDER V))
         (SETQ DE (VSDE_MK V (OFSF_REORDER F) (OFSF_REORDERL THEO) BVL))
         (VSDE_COMPUTE DE)
         (COND ((VSDE_TPLLINP DE BVL) (SETQ SLTD T)))
         (PROGN (SETQ DEL (CONS DE DEL)) DE)
         (SETKORDER OO))
        (GO WHILELABEL))
      (COND (SLTD (RETURN DE)))
      (RETURN (VSDB_BESTDE (REVERSIP DEL))))) 
(PUT 'VS_BLOCKMAINLOOP 'NUMBER-OF-ARGS 1) 
(DE VS_BLOCKMAINLOOP (DB)
    (PROG (ND VL F MBR)
      (PROG ()
       WHILELABEL
        (COND ((NOT (VSDB_TODOP DB)) (RETURN NIL)))
        (PROGN
         (SETQ ND (VSDB_WCGET DB))
         (SETQ VL (VSND_VL ND))
         (SETQ F (VSND_F ND))
         (COND ((VSND_FLG ND) (VSDB_APPLYVS DB ND))
               (T
                (PROGN
                 (SETQ MBR (VSDB_HTMEMBER DB F))
                 (COND
                  ((OR (NOT MBR) (VSVS_ARP (VSND_VS ND)))
                   (PROGN
                    (COND ((NOT MBR) (VSDB_HTINSERT DB F)))
                    (COND ((EQ F 'TRUE) (VSDB_DROPALL DB)))
                    (COND ((NULL VL) (VSDB_SCINSERT DB ND))
                          (T
                           (PROGN
                            (COND (*RLVERBOSE (VSND_PRINTINFO ND)))
                            (VSDB_EXPANDNODE DB ND)))))))))))
        (GO WHILELABEL)))) 
(PUT 'VSDB_APPLYVS 'NUMBER-OF-ARGS 2) 
(DE VSDB_APPLYVS (DB ND)
    (PROG (VS V OO FFL)
      (SETQ VS (VSND_VS ND))
      (SETQ V (VSVS_V VS))
      (SETQ OO (UPDKORDER V))
      (SETQ FFL
              (QFF_APPLYVS (VSVS_REORDER VS) (OFSF_REORDER (VSND_F ND))
               (VSDB_BVL DB) (OFSF_REORDERL (VSDB_CURTHEO DB))))
      (SETKORDER OO)
      (PROG (FF)
        (SETQ FF FFL)
       LAB
        (COND ((NULL FF) (RETURN NIL)))
        ((LAMBDA (FF)
           (VSDB_WCINSERT DB
            (VSND_MK NIL VS (VSND_VL ND) FF (VSND_PARENT ND) NIL)))
         (CAR FF))
        (SETQ FF (CDR FF))
        (GO LAB)))) 
(PUT 'VSDB_EXPANDNODE 'NUMBER-OF-ARGS 2) 
(DE VSDB_EXPANDNODE (DB ND)
    (PROG ()
      (COND (NIL NIL))
      (COND (NIL NIL))
      (COND ((VSDB_TRYEXPANDNO DB ND) (RETURN NIL)))
      (COND ((VSDB_TRYEXPANDDG DB ND) (RETURN NIL)))
      (COND ((EQN RLVARSELLVL* 1) (VSDB_EXPANDNODE1 DB ND)))
      (COND ((EQN RLVARSELLVL* 2) (VSDB_EXPANDNODE2 DB ND)))
      (COND ((EQN RLVARSELLVL* 3) (VSDB_EXPANDNODE3 DB ND)))
      (COND ((EQN RLVARSELLVL* 4) (VSDB_EXPANDNODE4 DB ND))))) 
(PUT 'VSDB_TRYEXPANDNO 'NUMBER-OF-ARGS 2) 
(DE VSDB_TRYEXPANDNO (DB ND)
    (PROG (VL RVL RES V)
      (SETQ VL (LTO_SETMINUS (VSND_VL ND) (VSND_TVL ND)))
      (SETQ RVL (CL_FVARL1 (VSND_F ND)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND VL (NOT RES))) (RETURN NIL)))
        (PROGN
         (SETQ V (PROG1 (CAR VL) (SETQ VL (CDR VL))))
         (COND
          ((NOT (MEMQ V RVL))
           (PROGN
            (SETQ RES T)
            (VSDB_WCINSERT DB
             (VSND_MK NIL (VSAR_MK V) (LTO_DELQ V (VSND_VL ND)) (VSND_F ND) ND
              (VSND_TVL ND)))))))
        (GO WHILELABEL))
      (COND
       ((AND *RLVERBOSE RES)
        (IOTO_TPRIN2T (LIST "+++++ SUCC " V " (does not occur)"))))
      (RETURN RES))) 
(PUT 'VSDB_TRYEXPANDDG 'NUMBER-OF-ARGS 2) 
(DE VSDB_TRYEXPANDDG (DB ND)
    (PROG (VL F RES V SV G)
      (SETQ G 0)
      (SETQ VL (LTO_SETMINUS (VSND_VL ND) (VSND_TVL ND)))
      (SETQ F (VSND_F ND))
      (COND
       ((VSVS_DGP (VSND_VS ND))
        (SETQ VL (LTO_DELQ (VSDG_SV (VSND_VS ND)) VL))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND VL (NOT RES))) (RETURN NIL)))
        (PROGN
         (SETQ V (PROG1 (CAR VL) (SETQ VL (CDR VL))))
         (SETQ G (VS_DGCD F V))
         (COND
          ((GREATERP G 1)
           (PROGN
            (SETQ RES T)
            (SETQ SV (VS_SHADOW V))
            (VSDB_WCINSERT DB
             (VSND_MK T (VSDG_MK V G SV) (SUBST SV V (VSND_VL ND)) (VSND_F ND)
              ND (VSND_TVL ND)))))))
        (GO WHILELABEL))
      (COND
       ((AND *RLVERBOSE RES)
        (IOTO_TPRIN2T
         (LIST "+++++ SUCC " V " ^ " G " = " SV " (degree shift)"))))
      (RETURN RES))) 
(PUT 'VSDB_EXPANDNODE1 'NUMBER-OF-ARGS 2) 
(DE VSDB_EXPANDNODE1 (DB ND)
    (PROG (V OO DE)
      (SETQ V (CAR (VSND_VL ND)))
      (SETQ OO (UPDKORDER V))
      (SETQ DE
              (VSDE_MK V (OFSF_REORDER (VSND_F ND))
               (OFSF_REORDERL (VSDB_CURTHEO DB)) (VSDB_BVL DB)))
      (VSDE_COMPUTE DE)
      (SETKORDER OO)
      (VSDB_INSERTAEC DB ND DE))) 
(PUT 'VSDB_EXPANDNODE2 'NUMBER-OF-ARGS 2) 
(DE VSDB_EXPANDNODE2 (DB ND)
    (PROG (VL F THEO BVL V OO DE)
      (SETQ VL (VSND_VL ND))
      (SETQ F (VSND_F ND))
      (SETQ THEO (VSDB_CURTHEO DB))
      (SETQ BVL (VSDB_BVL DB))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ V (PROG1 (CAR VL) (SETQ VL (CDR VL))))
         (SETQ OO (UPDKORDER V))
         (SETQ DE (VSDE_MK V (OFSF_REORDER F) (OFSF_REORDERL THEO) BVL))
         (VSDE_COMPUTE DE)
         (SETKORDER OO))
        (COND ((NOT (OR (NULL VL) (VSDE_TPL DE))) (GO REPEATLABEL))))
      (VSDB_INSERTAEC DB ND DE))) 
(PUT 'VSDB_EXPANDNODE3 'NUMBER-OF-ARGS 2) 
(DE VSDB_EXPANDNODE3 (DB ND)
    (PROG (VL F THEO BVL SLTD V OO DE DEL)
      (SETQ VL (VSND_VL ND))
      (SETQ F (VSND_F ND))
      (SETQ THEO (VSDB_CURTHEO DB))
      (SETQ BVL (VSDB_BVL DB))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND VL (NOT SLTD))) (RETURN NIL)))
        (PROGN
         (SETQ V (PROG1 (CAR VL) (SETQ VL (CDR VL))))
         (SETQ OO (UPDKORDER V))
         (SETQ DE (VSDE_MK V (OFSF_REORDER F) (OFSF_REORDERL THEO) BVL))
         (VSDE_COMPUTE DE)
         (COND ((VSDE_TPLLINP DE BVL) (SETQ SLTD T)))
         (PROGN (SETQ DEL (CONS DE DEL)) DE)
         (SETKORDER OO))
        (GO WHILELABEL))
      (COND (SLTD (PROGN (VSDB_INSERTAEC DB ND DE) (RETURN NIL))))
      (SETQ DE (VSDB_BESTDE (REVERSIP DEL)))
      (VSDB_INSERTAEC DB ND DE))) 
(PUT 'VSDB_EXPANDNODE4 'NUMBER-OF-ARGS 2) 
(DE VSDB_EXPANDNODE4 (DB ND) NIL) 
(PUT 'VSDB_BESTDE 'NUMBER-OF-ARGS 1) 
(DE VSDB_BESTDE (DEL)
    (PROG (TMPDEL SLTD DE OO)
      (COND (NIL NIL))
      (SETQ TMPDEL DEL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TMPDEL (NOT SLTD))) (RETURN NIL)))
        (PROGN
         (SETQ DE (PROG1 (CAR TMPDEL) (SETQ TMPDEL (CDR TMPDEL))))
         (SETQ OO (UPDKORDER (VSDE_V DE)))
         (COND ((VSDE_TPLLDP DE 1) (SETQ SLTD T)))
         (SETKORDER OO))
        (GO WHILELABEL))
      (COND (SLTD (RETURN DE)))
      (SETQ TMPDEL DEL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TMPDEL (NOT SLTD))) (RETURN NIL)))
        (PROGN
         (SETQ DE (PROG1 (CAR TMPDEL) (SETQ TMPDEL (CDR TMPDEL))))
         (SETQ OO (UPDKORDER (VSDE_V DE)))
         (COND ((VSDE_TPLLDP DE 2) (SETQ SLTD T)))
         (SETKORDER OO))
        (GO WHILELABEL))
      (COND (SLTD (RETURN DE)))
      (SETQ TMPDEL DEL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TMPDEL (NOT SLTD))) (RETURN NIL)))
        (PROGN
         (SETQ DE (PROG1 (CAR TMPDEL) (SETQ TMPDEL (CDR TMPDEL))))
         (SETQ OO (UPDKORDER (VSDE_V DE)))
         (COND ((VSDE_TPL DE) (SETQ SLTD T)))
         (SETKORDER OO))
        (GO WHILELABEL))
      (COND (SLTD (RETURN DE)))
      (RETURN (CAR DEL)))) 
(PUT 'VSDB_INSERTAEC 'NUMBER-OF-ARGS 3) 
(DE VSDB_INSERTAEC (DB ND DE)
    (PROG (TPL F V NVL)
      (SETQ TPL (VSDE_TPL DE))
      (COND
       ((NULL (VSDE_TPL DE))
        (PROGN
         (COND (*RLVERBOSE (IOTO_TPRIN2T (LIST "+++++ FAIL " (VSDE_V DE)))))
         (VSDB_FCINSERT DB ND)
         (RETURN NIL))))
      (COND
       (*RLVERBOSE
        (IOTO_TPRIN2T
         (LIST "+++++ SUCC " (VSDE_V DE) " (elimset size = "
               (LENGTH (VSDE_TPL DE)) ")"))))
      (SETQ F (VSDE_F DE))
      (SETQ V (VSDE_V DE))
      (SETQ NVL (LTO_DELQ V (VSND_VL ND)))
      (PROG (TP)
        (SETQ TP TPL)
       LAB
        (COND ((NULL TP) (RETURN NIL)))
        ((LAMBDA (TP)
           (VSDB_WCINSERT DB (VSND_MK T (VSTS_MK V TP) NVL F ND NIL)))
         (CAR TP))
        (SETQ TP (CDR TP))
        (GO LAB)))) 
(PUT 'VSDB_COLLECTRESULT 'NUMBER-OF-ARGS 1) 
(DE VSDB_COLLECTRESULT (DB)
    (PROG (FL VL)
      (COND (NIL NIL))
      (PROG (ND)
        (SETQ ND (VSDB_SC DB))
       LAB
        (COND ((NULL ND) (RETURN NIL)))
        ((LAMBDA (ND)
           (PROGN
            (COND (NIL NIL))
            (PROG (W1)
              (SETQ W1 (VSND_F ND))
              (SETQ FL (CONS W1 FL))
              (RETURN W1))))
         (CAR ND))
        (SETQ ND (CDR ND))
        (GO LAB))
      (PROG (ND)
        (SETQ ND (VSDB_FC DB))
       LAB
        (COND ((NULL ND) (RETURN NIL)))
        ((LAMBDA (ND)
           (PROGN
            (COND (NIL NIL))
            (PROG (W1)
              (SETQ W1 (VSND_F ND))
              (SETQ FL (CONS W1 FL))
              (RETURN W1))
            (SETQ VL (UNION VL (VSND_VL ND)))))
         (CAR ND))
        (SETQ ND (CDR ND))
        (GO LAB))
      (RETURN (CONS (CL_SIMPL (CONS 'OR FL) (VSDB_THEO DB) (MINUS 1)) VL)))) 
(PUT 'VS_SHADOW 'NUMBER-OF-ARGS 1) 
(DE VS_SHADOW (X)
    (PROG (RES) (SETQ RES (GENSYM)) (PUT RES 'SHADOW X) (RETURN RES))) 
(PUT 'VS_DGCD 'NUMBER-OF-ARGS 2) 
(DE VS_DGCD (F X)
    (PROG (ATL AT G)
      (SETQ G 0)
      (SETQ ATL (CL_ATL1 F))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND ATL (NOT (EQN G 1)))) (RETURN NIL)))
        (PROGN
         (SETQ AT (PROG1 (CAR ATL) (SETQ ATL (CDR ATL))))
         (SETQ G (GCDN G (SFTO_DGCDF (CADR AT) X))))
        (GO WHILELABEL))
      (RETURN G))) 
(PUT 'VSDB_2GML 'NUMBER-OF-ARGS 2) 
(DE VSDB_2GML (DB FILENAME)
    (PROG (NAT VLM)
      (SETQ NAT *NAT)
      (OFF1 'NAT)
      (OUT (LIST FILENAME))
      (IOTO_PRIN2T "Creator \"REDLOG\"")
      (IOTO_PRIN2T "graph [")
      (IOTO_PRIN2T "label \"VSdb graph generated by REDLOG.\"")
      (IOTO_PRIN2T "directed 1")
      (SETQ VLM (CONS NIL 0))
      (PROG (ND)
        (SETQ ND (VSDB_WC DB))
       LAB
        (COND ((NULL ND) (RETURN NIL)))
        ((LAMBDA (ND)
           (SETQ VLM (VSDB_2GMLN ND (PLUS (CDR VLM) 1) "#C0C0C0" VLM)))
         (CAR ND))
        (SETQ ND (CDR ND))
        (GO LAB))
      (PROG (ND)
        (SETQ ND (VSDB_SC DB))
       LAB
        (COND ((NULL ND) (RETURN NIL)))
        ((LAMBDA (ND)
           (SETQ VLM (VSDB_2GMLN ND (PLUS (CDR VLM) 1) "#00FF00" VLM)))
         (CAR ND))
        (SETQ ND (CDR ND))
        (GO LAB))
      (PROG (ND)
        (SETQ ND (VSDB_FC DB))
       LAB
        (COND ((NULL ND) (RETURN NIL)))
        ((LAMBDA (ND)
           (SETQ VLM (VSDB_2GMLN ND (PLUS (CDR VLM) 1) "#FF0000" VLM)))
         (CAR ND))
        (SETQ ND (CDR ND))
        (GO LAB))
      (IOTO_PRIN2T "]")
      (COND (NAT (ON1 'NAT)))
      (SHUT (LIST FILENAME)))) 
(PUT 'VSDB_2GMLN 'NUMBER-OF-ARGS 4) 
(DE VSDB_2GMLN (ND N C VLM)
    (PROG (W P)
      (SETQ W (ATSOC ND (CAR VLM)))
      (COND (W (RETURN VLM)))
      (VSDB_2GMLN-PRINTN ND N C)
      (SETQ P (VSND_PARENT ND))
      (COND
       (P
        (PROGN
         (SETQ VLM (VSDB_2GMLN P (PLUS N 1) "#C0C0C0" VLM))
         (SETQ W (ATSOC P (CAR VLM)))
         (COND (NIL NIL))
         (VSDB_2GMLN-PRINTE ND (CDR W) N))))
      (RETURN
       (CONS (CONS (CONS ND N) (CAR VLM))
             (COND ((GREATERP N (CDR VLM)) N) (T (CDR VLM))))))) 
(PUT 'VSDB_2GMLN-PRINTN 'NUMBER-OF-ARGS 3) 
(DE VSDB_2GMLN-PRINTN (ND N C)
    (PROGN
     (IOTO_PRIN2T "node [")
     (IOTO_PRIN2T (LIST "id " N))
     (IOTO_PRIN2T "label \"")
     (MATHPRINT (RL_PREPFOF (VSND_F ND)))
     (IOTO_PRIN2T (LIST "vl: " (VSND_VL ND)))
     (IOTO_PRIN2T "\"")
     (IOTO_PRIN2T (LIST "graphics [" "fill \"" C "\"]"))
     (IOTO_PRIN2T "]"))) 
(PUT 'VSDB_2GMLN-PRINTE 'NUMBER-OF-ARGS 3) 
(DE VSDB_2GMLN-PRINTE (ND SS TT)
    (PROG (VS TP)
      (SETQ VS (VSND_VS ND))
      (IOTO_PRIN2T "edge [")
      (IOTO_PRIN2T (LIST "source " SS))
      (IOTO_PRIN2T (LIST "target " TT))
      (IOTO_PRIN2T "label \"")
      (COND
       ((VSVS_TSP VS)
        (PROGN
         (SETQ TP (VSTS_TP VS))
         (COND
          ((EQ (VSTP_NP TP) 'MINF)
           (PROGN
            (IOTO_PRIN2T (LIST (VSVS_V VS) " = - inf"))
            (IOTO_PRIN2 "guard:")
            (MATHPRINT (RL_PREPFOF (VSTP_GUARD TP)))))
          ((EQ (VSTP_NP TP) 'PINF)
           (PROGN
            (IOTO_PRIN2T (LIST (VSVS_V VS) " = + inf"))
            (IOTO_PRIN2 "guard:")
            (MATHPRINT (RL_PREPFOF (VSTP_GUARD TP)))))
          ((EQ (VSTP_NP TP) 'MEPS)
           (PROGN
            (IOTO_PRIN2T (LIST (VSVS_V VS) " = tp - eps"))
            (MATHPRINT (PREPF (VSPR_F (VSTP_PR TP))))
            (IOTO_PRIN2T (VSPR_RSL (VSTP_PR TP)))
            (IOTO_PRIN2 "guard:")
            (MATHPRINT (RL_PREPFOF (VSTP_GUARD TP)))))
          ((EQ (VSTP_NP TP) 'PEPS)
           (PROGN
            (IOTO_PRIN2T (LIST (VSVS_V VS) " = tp + eps"))
            (MATHPRINT (PREPF (VSPR_F (VSTP_PR TP))))
            (IOTO_PRIN2T (VSPR_RSL (VSTP_PR TP)))
            (IOTO_PRIN2 "guard:")
            (MATHPRINT (RL_PREPFOF (VSTP_GUARD TP)))))
          (T
           (PROGN
            (IOTO_PRIN2T (LIST (VSVS_V VS) " = tp"))
            (MATHPRINT (PREPF (VSPR_F (VSTP_PR TP))))
            (IOTO_PRIN2T (VSPR_RSL (VSTP_PR TP)))
            (IOTO_PRIN2 "guard:")
            (MATHPRINT (RL_PREPFOF (VSTP_GUARD TP))))))))
       ((VSVS_DGP VS)
        (IOTO_PRIN2T
         (LIST (VSVS_V VS) " = " (VSDG_G VS) "-th root of " (VSDG_SV VS))))
       ((VSVS_ARP VS) (IOTO_PRIN2T (LIST (VSVS_V VS) " = arbitrary"))))
      (IOTO_PRIN2T "\"")
      (IOTO_PRIN2T "]"))) 
(PUT 'VSDB_PRINTS 'NUMBER-OF-ARGS 1) 
(DE VSDB_PRINTS (DB)
    (PROGN
     (IOTO_PRIN2 "VSdb: ")
     (IOTO_PRIN2T
      (LIST "#W: " (VSCO_LENGTH (VSDB_WC DB)) " #S: "
            (VSCO_LENGTH (VSDB_SC DB)) " #F: " (VSCO_LENGTH (VSDB_FC DB))
            " #H: " (LENGTH (CADR (VSDB_HT DB))))))) 
(PUT 'VSDB_PRINT 'NUMBER-OF-ARGS 1) 
(DE VSDB_PRINT (DB)
    (PROG (WC SC FC)
      (SETQ WC (VSDB_WC DB))
      (SETQ SC (VSDB_SC DB))
      (SETQ FC (VSDB_FC DB))
      (IOTO_PRIN2T NIL)
      (IOTO_PRIN2 "VSdb WORKING NODES:")
      (COND ((NULL WC) (IOTO_PRIN2T " NONE"))
            (T (PROGN (IOTO_PRIN2T NIL) (VSND_PRINTL WC))))
      (IOTO_PRIN2T NIL)
      (IOTO_PRIN2 "VSdb SUCCESS NODES:")
      (COND ((NULL SC) (IOTO_PRIN2T " NONE"))
            (T (PROGN (IOTO_PRIN2T NIL) (VSND_PRINTL SC))))
      (IOTO_PRIN2T NIL)
      (IOTO_PRIN2 "VSdb FAILURE NODES:")
      (COND ((NULL FC) (IOTO_PRIN2T " NONE"))
            (T (PROGN (IOTO_PRIN2T NIL) (VSND_PRINTL FC)))))) 
(PUT 'VSND_PRINTL 'NUMBER-OF-ARGS 1) 
(DE VSND_PRINTL (NDL)
    (PROG (ND)
      (SETQ ND NDL)
     LAB
      (COND ((NULL ND) (RETURN NIL)))
      ((LAMBDA (ND) (VSND_PRINT ND)) (CAR ND))
      (SETQ ND (CDR ND))
      (GO LAB))) 
(PUT 'VSND_PRINTS 'NUMBER-OF-ARGS 1) 
(DE VSND_PRINTS (ND)
    (IOTO_PRIN2T (LIST "VSnd:" " vl: " (VSND_VL ND) " flag: " (VSND_FLG ND)))) 
(PUT 'VSND_PRINT 'NUMBER-OF-ARGS 1) 
(DE VSND_PRINT (ND)
    (PROG (VS)
      (IOTO_PRIN2T "VSnd:")
      (MATHPRINT (RL_PREPFOF (VSND_F ND)))
      (IOTO_PRIN2T (LIST "vl: " (VSND_VL ND) " flag: " (VSND_FLG ND)))
      (SETQ VS (VSND_VS ND))
      (VSVS_PRINTS VS))) 
(PUT 'VSND_PRINTINFO 'NUMBER-OF-ARGS 1) 
(DE VSND_PRINTINFO (ND)
    (PROG (F)
      (SETQ F (VSND_F ND))
      (IOTO_TPRIN2 "+++++ QE node: ")
      (IOTO_PRIN2 (LIST "#q-var = " (LENGTH (VSND_VL ND)) ", "))
      (IOTO_PRIN2T (LIST "#at = " (CL_ATNUM F)))
      (IOTO_PRIN2T NIL))) 
(ENDMODULE) 