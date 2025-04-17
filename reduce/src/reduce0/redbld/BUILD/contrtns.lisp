(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CONTRTNS)) 
(GLOBAL '(DIMEX* SGN* SIGNAT* SPACES* NUMINDXL* PAIR_ID_NUM*)) 
(SETQ PAIR_ID_NUM*
        '((|0| . 0) (|1| . 1) (|2| . 2) (|3| . 3) (|4| . 4) (|5| . 5) (|6| . 6)
          (|7| . 7) (|8| . 8) (|9| . 9) (|10| . 10) (|11| . 11) (|12| . 12)
          (|13| . 13))) 
(FLUID '(DUMMY_ID* G_DVNAMES EPSILON* *DISTRIBUTE)) 
(SWITCH (LIST 'ONESPACE)) 
(SETQ *ONESPACE T) 
(FLUID '(INDXL_TENS* DUMMY_ID* G_DVNAMES)) 
(PUT 'NO_DUM_VARP 'NUMBER-OF-ARGS 1) 
(PUT 'NO_DUM_VARP 'DEFINED-ON-LINE '54) 
(PUT 'NO_DUM_VARP 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'NO_DUM_VARP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NO_DUM_VARP (U)
    (COND ((OR (NULL (CDR U)) (EQUAL (|SPLITLIST:| (CDR U) 'LIST) (CDR U))) T)
          (T NIL))) 
(PUT 'SEP_TENS_FROM_OTHER 'NUMBER-OF-ARGS 1) 
(PUT 'SEP_TENS_FROM_OTHER 'DEFINED-ON-LINE '86) 
(PUT 'SEP_TENS_FROM_OTHER 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'SEP_TENS_FROM_OTHER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SEP_TENS_FROM_OTHER (U)
    (PROG (MV TEL OTHER Y)
      (SETQ OTHER (COND ((ZEROP 1) NIL) (T 1)))
     L
      (COND
       ((NUMBERP U)
        (RETURN
         (LIST (REVERSIP TEL)
               ((LAMBDA (G585)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF OTHER G585))
                        (T (POLY-MULTF OTHER G585))))
                (COND ((ZEROP U) NIL) (T U))))))
       ((ATOM (CAAAR U))
        (SETQ OTHER
                ((LAMBDA (G587)
                   (COND (*PHYSOP-LOADED (PHYSOP-MULTF OTHER G587))
                         (T (POLY-MULTF OTHER G587))))
                 (LIST (CONS (CAAR U) 1)))))
       (T
        (PROGN
         (COND
          ((SETQ Y (GET (CAR (CAAAR U)) 'TRANSLATE1))
           (PROGN
            (SETQ U (FULLCOPY U))
            (SETCAR (CAAR U) (APPLY1 Y (CAAAR U))))))
         (COND
          ((TENSORP (SETQ MV (CAAAR U)))
           (COND
            ((OR (NULL (NO_DUM_VARP MV)) (FLAGP (CAR MV) 'NONCOM))
             (SETQ TEL (CONS (CAAAR U) TEL)))
            (T
             (SETQ OTHER
                     ((LAMBDA (G589)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF OTHER G589))
                              (T (POLY-MULTF OTHER G589))))
                      (LIST (CONS (CAAR U) 1)))))))
          (T
           (SETQ OTHER
                   ((LAMBDA (G591)
                      (COND (*PHYSOP-LOADED (PHYSOP-MULTF OTHER G591))
                            (T (POLY-MULTF OTHER G591))))
                    (LIST (CONS (CAAR U) 1)))))))))
      (SETQ U (CDAR U))
      (GO L))) 
(PUT 'ALL_INDEX_LST 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_INDEX_LST 'DEFINED-ON-LINE '113) 
(PUT 'ALL_INDEX_LST 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'ALL_INDEX_LST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_INDEX_LST (U)
    (COND ((NULL U) NIL)
          (T
           (APPEND
            ((LAMBDA (Y)
               (COND ((AND (LISTP (CAR Y)) (EQUAL (CAAR Y) 'LIST)) (CDR Y))
                     (T Y)))
             (CDAR U))
            (ALL_INDEX_LST (CDR U)))))) 
(PUT 'DEL_AFFIN_TENS 'NUMBER-OF-ARGS 1) 
(PUT 'DEL_AFFIN_TENS 'DEFINED-ON-LINE '127) 
(PUT 'DEL_AFFIN_TENS 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'DEL_AFFIN_TENS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DEL_AFFIN_TENS (U)
    (COND ((NULL U) NIL) ((AFFINEP (CAR U)) (DEL_AFFIN_TENS (CDR U)))
          (T (CONS (CAR U) (DEL_AFFIN_TENS (CDR U)))))) 
(PUT 'DV_CANON_COVCONT 'NUMBER-OF-ARGS 1) 
(PUT 'DV_CANON_COVCONT 'DEFINED-ON-LINE '134) 
(PUT 'DV_CANON_COVCONT 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'DV_CANON_COVCONT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_CANON_COVCONT (SF)
    (COND ((OR (ATOM SF) (ATOM (CAR SF))) SF)
          (T
           (PROG (TENSLIST IDLIST DUMMYID)
             (SETQ DUMMYID DUMMY_ID*)
             (SETQ TENSLIST (CAR (SEP_TENS_FROM_OTHER SF)))
             (COND ((NULL TENSLIST) (RETURN (RESTOREALLDFS SF))))
             (SETQ IDLIST (ALL_INDEX_LST TENSLIST))
             (PROG (Z)
               (SETQ Z TENSLIST)
              LAB
               (COND ((NULL Z) (RETURN NIL)))
               ((LAMBDA (Z)
                  (COND
                   ((OR (EQUAL (GET (CAR Z) 'PARTIC_TENS) 'SIMPDEL)
                        (AFFINEP Z))
                    (PROG (Y)
                      (SETQ Y (CDR Z))
                     LAB
                      (COND ((NULL Y) (RETURN NIL)))
                      ((LAMBDA (Y)
                         (SETQ DUMMYID (DELETE (|RAISEIND:| Y) DUMMYID)))
                       (CAR Y))
                      (SETQ Y (CDR Y))
                      (GO LAB)))))
                (CAR Z))
               (SETQ Z (CDR Z))
               (GO LAB))
             (PROG (Z)
               (SETQ Z IDLIST)
              LAB
               (COND ((NULL Z) (RETURN NIL)))
               ((LAMBDA (Z)
                  (COND
                   ((ATOM Z)
                    (COND
                     ((MEMQ Z DUMMYID) (SETQ DUMMYID (DELETE Z DUMMYID)))))
                   ((AND (EQCAR Z 'MINUS) (MEMQ (CADR Z) DUMMYID))
                    (PROGN
                     (SETQ SF (SUBST (LIST 'MINUS (CADR Z)) (CADR Z) SF))
                     (SETQ DUMMYID (DELETE (CADR Z) DUMMYID))))))
                (CAR Z))
               (SETQ Z (CDR Z))
               (GO LAB))
             (RETURN (RESTOREALLDFS SF)))))) 
(PUT 'COV_CONTP 'NUMBER-OF-ARGS 2) 
(PUT 'COV_CONTP 'DEFINED-ON-LINE '161) 
(PUT 'COV_CONTP 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'COV_CONTP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COV_CONTP (U V)
    (OR (AND (COV_LST_IDSP U) (CONT_LST_IDSP V))
        (AND (CONT_LST_IDSP U) (COV_LST_IDSP V)))) 
(PUT 'BELONG_TO_SPACEP 'NUMBER-OF-ARGS 2) 
(PUT 'BELONG_TO_SPACEP 'DEFINED-ON-LINE '170) 
(PUT 'BELONG_TO_SPACEP 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'BELONG_TO_SPACEP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE BELONG_TO_SPACEP (U SP)
    (COND ((OR (NULL U) (EQUAL SP 'WHOLESPACE)) T)
          ((EQ (GET (CAR U) 'SPACE) SP) (BELONG_TO_SPACEP (CDR U) SP)))) 
(PUT 'EXTRACT_TENS 'NUMBER-OF-ARGS 2) 
(PUT 'EXTRACT_TENS 'DEFINED-ON-LINE '179) 
(PUT 'EXTRACT_TENS 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'EXTRACT_TENS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXTRACT_TENS (TEL SP_TENS)
    (COND ((NULL TEL) NIL)
          ((EQUAL (CAAR TEL) SP_TENS)
           (CONS (CAR TEL) (EXTRACT_TENS (CDR TEL) SP_TENS)))
          (T (EXTRACT_TENS (CDR TEL) SP_TENS)))) 
(PUT 'TREAT_DUMMY_IDS 'NUMBER-OF-ARGS 1) 
(PUT 'TREAT_DUMMY_IDS 'DEFINED-ON-LINE '191) 
(PUT 'TREAT_DUMMY_IDS 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'TREAT_DUMMY_IDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TREAT_DUMMY_IDS (SF)
    (COND
     (*ONESPACE
      (PROG (USER_G_DVNAMES RES)
        (SETQ USER_G_DVNAMES G_DVNAMES)
        (DUMMY_NAM DUMMY_ID*)
        (SETQ RES (DV_CANON_MONOMIAL SF))
        (SETQ G_DVNAMES USER_G_DVNAMES)
        (RETURN
         (COND (G_DVNAMES (DV_CANON_COVCONT (DV_CANON_MONOMIAL RES)))
               (T (DV_CANON_COVCONT RES))))))
     (T
      (PROG (RES PARTIT_SPACE_LST IDXL SP USER_G_DVNAMES BOOL)
        (SETQ PARTIT_SPACE_LST NIL)
        (SETQ USER_G_DVNAMES G_DVNAMES)
        (SETQ PARTIT_SPACE_LST
                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Y SPACES*)
                  (COND ((NULL Y) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (Y) (CONS (CAR Y) NIL)) (CAR Y))
                                   NIL)))
                 LOOPLABEL
                  (SETQ Y (CDR Y))
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (Y) (CONS (CAR Y) NIL)) (CAR Y)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (PROG (Z)
          (SETQ Z DUMMY_ID*)
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z)
             (COND
              ((SETQ SP (SPACE_OF_IDX Z))
               (COND
                ((SETQ IDXL (ASSOC SP PARTIT_SPACE_LST))
                 (SETCDR IDXL (CONS Z (CDR IDXL))))
                (T
                 (RERROR 'CANTENS 14
                         (LIST "Index " Z
                               " does not belong to a defined space")))))))
           (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB))
        (SETQ RES SF)
        (PROG (Z)
          (SETQ Z PARTIT_SPACE_LST)
         LAB
          (COND ((NULL Z) (RETURN NIL)))
          ((LAMBDA (Z)
             (COND
              ((SETQ IDXL (CDR Z))
               (PROGN
                (SETQ BOOL T)
                (DUMMY_NAM IDXL)
                (SETQ RES (DV_CANON_MONOMIAL RES))))))
           (CAR Z))
          (SETQ Z (CDR Z))
          (GO LAB))
        (COND ((NOT BOOL) (SETQ RES (DV_CANON_MONOMIAL RES))))
        (SETQ G_DVNAMES USER_G_DVNAMES)
        (RETURN
         (COND (G_DVNAMES (DV_CANON_COVCONT (DV_CANON_MONOMIAL RES)))
               (T (DV_CANON_COVCONT RES)))))))) 
(PUT 'CANONICAL 'NUMBER-OF-ARGS 1) 
(PUT 'CANONICAL 'DEFINED-ON-LINE '237) 
(PUT 'CANONICAL 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'CANONICAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CANONICAL (SQ)
    (PROG (SF DENOM *DISTRIBUTE)
      (SETQ SQ (SIMP* (CAR SQ)))
      (SETQ DENOM (CDR SQ))
      (ON (LIST 'DISTRIBUTE))
      (SETQ SF (DISTRI_POL (CAR SQ)))
      (RETURN
       (SIMP*
        (LIST '*SQ (CONS (CANONICAL1 SF (CADR (CHECK_IDS SF))) DENOM) NIL))))) 
(PUT 'CANONICAL1 'NUMBER-OF-ARGS 2) 
(PUT 'CANONICAL1 'DEFINED-ON-LINE '249) 
(PUT 'CANONICAL1 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'CANONICAL1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CANONICAL1 (SF DUMLIST)
    (PROG (DUMMY_ID* RES)
      (SETQ DUMMY_ID* DUMLIST)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM SF) (ATOM (CAR SF))))) (RETURN NIL)))
        (PROGN
         (SETQ RES (ADDF RES (SIMPEPSI_MON_EXPR (CONS (CAR SF) NIL))))
         (SETQ SF (CDR SF))
         NIL)
        (GO WHILELABEL))
      (SETQ SF (DISTRI_POL (ADDF RES SF)))
      (SETQ RES NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (ATOM SF) (ATOM (CAR SF))))) (RETURN NIL)))
        (PROGN
         ((LAMBDA (Y)
            (COND
             ((GEQ (LENGTH (CAR Y)) 2)
              (SETQ RES (ADDF RES (DV_CANON_TENSOR Y))))
             (T (SETQ RES (ADDF RES (TREAT_DUMMY_IDS (CONS (CAR SF) NIL)))))))
          (SEP_TENS_FROM_OTHER (CONS (CAR SF) NIL)))
         (SETQ SF (CDR SF))
         NIL)
        (GO WHILELABEL))
      (CLEARALLNEWIDS)
      (RETURN (SETQ RES (ADDF RES SF))))) 
(PUT 'TENSOR_HAS_DUMMY_IDX 'NUMBER-OF-ARGS 2) 
(PUT 'TENSOR_HAS_DUMMY_IDX 'DEFINED-ON-LINE '283) 
(PUT 'TENSOR_HAS_DUMMY_IDX 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'TENSOR_HAS_DUMMY_IDX 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TENSOR_HAS_DUMMY_IDX (DUM TE)
    (COND ((NULL DUM) NIL) ((SMEMBER (CAR DUM) TE) T)
          (T (TENSOR_HAS_DUMMY_IDX (CDR DUM) TE)))) 
(PUT 'TENS_LIST_IS_GENERIC 'NUMBER-OF-ARGS 1) 
(PUT 'TENS_LIST_IS_GENERIC 'DEFINED-ON-LINE '292) 
(PUT 'TENS_LIST_IS_GENERIC 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'TENS_LIST_IS_GENERIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TENS_LIST_IS_GENERIC (TEL)
    (COND ((NULL TEL) T)
          ((NULL (GET (CAAR TEL) 'PARTIC_TENS))
           (TENS_LIST_IS_GENERIC (CDR TEL))))) 
(PUT 'MK_DELTA_FIRST 'NUMBER-OF-ARGS 1) 
(PUT 'MK_DELTA_FIRST 'DEFINED-ON-LINE '299) 
(PUT 'MK_DELTA_FIRST 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'MK_DELTA_FIRST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_DELTA_FIRST (TEL)
    (PROG (X Y Z)
      (SETQ X (EXTRACT_TENS TEL (GET 'DELTA 'NAME)))
      (SETQ Z (SETDIFF TEL X))
      (SETQ Y (EXTRACT_TENS Z (GET 'ETA 'NAME)))
      (SETQ Z (SETDIFF Z Y))
      (RETURN (APPEND X (APPEND Y Z))))) 
(PUT 'DV_CANON_TENSOR 'NUMBER-OF-ARGS 1) 
(PUT 'DV_CANON_TENSOR 'DEFINED-ON-LINE '313) 
(PUT 'DV_CANON_TENSOR 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'DV_CANON_TENSOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DV_CANON_TENSOR (U)
    (PROG (X TEL TEL_DUM TEL_FREE NOTENS)
      (SETQ TEL (CAR U))
      (SETQ TEL_FREE (COND ((ZEROP 1) NIL) (T 1)))
      (SETQ NOTENS (CADR U))
      (PROG (Y)
        (SETQ Y TEL)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (COND
            ((TENSOR_HAS_DUMMY_IDX DUMMY_ID* Y)
             (SETQ TEL_DUM (CONS Y TEL_DUM)))
            (T
             (SETQ TEL_FREE
                     ((LAMBDA (G592)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G592 TEL_FREE))
                              (T (POLY-MULTF G592 TEL_FREE))))
                      (LIST (CONS (GETPOWER (FKERN Y) 1) 1)))))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (SETQ TEL_DUM TEL_DUM)
      (RETURN
       (COND
        ((TENS_LIST_IS_GENERIC TEL_DUM)
         (PROGN
          (SETQ X (COND ((ZEROP 1) NIL) (T 1)))
          (COND
           (TEL_DUM
            (SETQ TEL_DUM
                    (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                      (SETQ Y TEL_DUM)
                      (COND ((NULL Y) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Y)
                                          (LIST
                                           (CONS (GETPOWER (FKERN Y) 1) 1)))
                                        (CAR Y))
                                       NIL)))
                     LOOPLABEL
                      (SETQ Y (CDR Y))
                      (COND ((NULL Y) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (Y)
                                  (LIST (CONS (GETPOWER (FKERN Y) 1) 1)))
                                (CAR Y))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))
          (PROG ()
           WHILELABEL
            (COND ((NOT TEL_DUM) (RETURN NIL)))
            (PROGN
             (SETQ X
                     (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR TEL_DUM) X))
                           (T (POLY-MULTF (CAR TEL_DUM) X))))
             (SETQ TEL_DUM (CDR TEL_DUM))
             NIL)
            (GO WHILELABEL))
          ((LAMBDA (G594 G595)
             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G594 G595))
                   (T (POLY-MULTF G594 G595))))
           (RESTOREALLDFS TEL_FREE)
           (TREAT_DUMMY_IDS
            (COND (*PHYSOP-LOADED (PHYSOP-MULTF X NOTENS))
                  (T (POLY-MULTF X NOTENS)))))))
        (T
         ((LAMBDA (G598 G599)
            (COND (*PHYSOP-LOADED (PHYSOP-MULTF G598 G599))
                  (T (POLY-MULTF G598 G599))))
          (RESTOREALLDFS TEL_FREE)
          (TREAT_DUMMY_IDS
           ((LAMBDA (G596)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF G596 NOTENS))
                    (T (POLY-MULTF G596 NOTENS))))
            (SIMPTENSEXPR (MK_DELTA_FIRST TEL_DUM) DUMMY_ID* 1))))))))) 
(PUT 'SIMPTENSEXPR 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPTENSEXPR 'DEFINED-ON-LINE '347) 
(PUT 'SIMPTENSEXPR 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'SIMPTENSEXPR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPTENSEXPR (TEL DUM I)
    (PROG (RES)
      (SETQ RES (COND ((ZEROP 1) NIL) (T 1)))
      (RETURN
       (COND ((NUMBERP TEL) (COND ((ZEROP TEL) NIL) (T TEL)))
             ((OR (ATOM TEL) (EQUAL (LENGTH TEL) 1))
              (LIST (CONS (CONS (CAR TEL) 1) 1)))
             ((GEQ I (PLUS (LENGTH TEL) 1))
              (PROGN
               (PROG (I)
                 (SETQ I TEL)
                LAB
                 (COND ((NULL I) (RETURN NIL)))
                 ((LAMBDA (I)
                    (SETQ RES
                            ((LAMBDA (G601)
                               (COND (*PHYSOP-LOADED (PHYSOP-MULTF RES G601))
                                     (T (POLY-MULTF RES G601))))
                             (LIST (CONS (CONS I 1) 1)))))
                  (CAR I))
                 (SETQ I (CDR I))
                 (GO LAB))
               RES))
             (T
              ((LAMBDA (Y)
                 (COND
                  ((MEMQ Y (LIST 'SIMPDELT 'SIMPETA 'SIMPMETRIC))
                   (SIMPDELTETAEXPR TEL DUM I))
                  (T (SIMPTENSEXPR TEL DUM (PLUS I 1)))))
               (GET (CAR (NTH TEL I)) 'PARTIC_TENS))))))) 
(PUT 'SIMPDELTETAEXPR 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPDELTETAEXPR 'DEFINED-ON-LINE '370) 
(PUT 'SIMPDELTETAEXPR 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'SIMPDELTETAEXPR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPDELTETAEXPR (TEL DUM I)
    (PROG (ITEL RTEL RES OLD NEW)
      (SETQ ITEL (NTH TEL I))
      (COND
       ((AND
         (NEQ
          (COND ((EQCAR (CADR ITEL) 'MINUS) (CADR (CADR ITEL)))
                (T (LIST 'MINUS (CADR ITEL))))
          (CADDR ITEL))
         (INTERSECTION (FLATINDXL (CDR ITEL)) DUM))
        (PROGN
         (SETQ RTEL (REMOVE TEL I))
         (COND
          ((MEMQ (SETQ OLD (|RAISEIND:| (CADR ITEL))) DUM)
           (PROGN
            (SETQ OLD
                    (COND ((EQCAR (CADR ITEL) 'MINUS) (CADR (CADR ITEL)))
                          (T (LIST 'MINUS (CADR ITEL)))))
            (SETQ NEW (CADDR ITEL))))
          (T
           (PROGN
            (SETQ OLD
                    ((LAMBDA (U)
                       (COND ((EQCAR U 'MINUS) (CADR U)) (T (LIST 'MINUS U))))
                     (CADDR ITEL)))
            (SETQ NEW (CADR ITEL)))))
         (SETQ RES (SUBST NEW OLD RTEL))
         (RETURN (SIMPTENSEXPR RES DUM I))))
       (T (RETURN (SIMPTENSEXPR TEL DUM (PLUS I 1))))))) 
(PUT 'SELECT_EPSI_PAIRS 'NUMBER-OF-ARGS 1) 
(PUT 'SELECT_EPSI_PAIRS 'DEFINED-ON-LINE '395) 
(PUT 'SELECT_EPSI_PAIRS 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'SELECT_EPSI_PAIRS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SELECT_EPSI_PAIRS (EP)
    (COND ((NULL EP) NIL)
          (T
           ((LAMBDA (X)
              (COND
               ((AND (EQUAL (LENGTH X) 2) (COV_CONTP (CDAR X) (CDADR X)))
                (CONS X (SELECT_EPSI_PAIRS (CDR EP))))
               (T (SELECT_EPSI_PAIRS (CDR EP)))))
            (CAR EP))))) 
(PUT 'MK_EPS_LST 'NUMBER-OF-ARGS 1) 
(PUT 'MK_EPS_LST 'DEFINED-ON-LINE '410) 
(PUT 'MK_EPS_LST 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'MK_EPS_LST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MK_EPS_LST (TKL)
    (PROG (EPS_LST)
      (SETQ EPS_LST
              (COND
               ((AND *ONESPACE (GET 'EPSILON 'NAME))
                (LIST (EXTRACT_TENS TKL (FIND_NAME 'EPSILON))))
               (EPSILON*
                (PROG (I FORALL-RESULT FORALL-ENDPTR)
                  (SETQ I EPSILON*)
                  (COND ((NULL I) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (I) (EXTRACT_TENS TKL (CAR I)))
                                    (CAR I))
                                   NIL)))
                 LOOPLABEL
                  (SETQ I (CDR I))
                  (COND ((NULL I) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (I) (EXTRACT_TENS TKL (CAR I))) (CAR I))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               (T NIL)))
      (SETQ EPS_LST (SELECT_EPSI_PAIRS EPS_LST))
      (COND ((NULL EPS_LST) (RETURN (LIST NIL TKL))))
      (PROG (J)
        (SETQ J EPS_LST)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J) (SETQ TKL (SETDIFF TKL J))) (CAR J))
        (SETQ J (CDR J))
        (GO LAB))
      (RETURN (LIST EPS_LST TKL)))) 
(PUT '|GET_SIGN_SPACE:| 'NUMBER-OF-ARGS 1) 
(PUT '|GET_SIGN_SPACE:| 'DEFINED-ON-LINE '429) 
(PUT '|GET_SIGN_SPACE:| 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT '|GET_SIGN_SPACE:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |GET_SIGN_SPACE:| (U)
    (COND ((NULL U) (SIGNATURE '?)) (T (GET_SIGN_SPACE U)))) 
(PUT 'EPSI_TO_DEL 'NUMBER-OF-ARGS 1) 
(PUT 'EPSI_TO_DEL 'DEFINED-ON-LINE '433) 
(PUT 'EPSI_TO_DEL 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'EPSI_TO_DEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EPSI_TO_DEL (EP)
    (COND ((NULL EP) NIL)
          (T
           (PROG (DEL_PRD X Y)
             (SETQ DEL_PRD (COND ((ZEROP 1) NIL) (T 1)))
             (PROG (J)
               (SETQ J EP)
              LAB
               (COND ((NULL J) (RETURN NIL)))
               ((LAMBDA (J)
                  (PROGN
                   (SETQ X (ALL_INDEX_LST J))
                   (COND
                    ((EQUAL
                      (|GET_SIGN_SPACE:|
                       (COND ((SETQ Y (ASSOC (CAAR J) EPSILON*)) (CDR Y))
                             (T NIL)))
                      1)
                     (SETQ DEL_PRD
                             ((LAMBDA (G602)
                                (COND
                                 (*PHYSOP-LOADED (PHYSOP-MULTF G602 DEL_PRD))
                                 (T (POLY-MULTF G602 DEL_PRD))))
                              (NEGF
                               (APPLY1 'SIMPDEL (CONS (FIND_NAME 'DEL) X))))))
                    (T
                     (SETQ DEL_PRD
                             ((LAMBDA (G604)
                                (COND
                                 (*PHYSOP-LOADED (PHYSOP-MULTF G604 DEL_PRD))
                                 (T (POLY-MULTF G604 DEL_PRD))))
                              (APPLY1 'SIMPDEL (CONS (FIND_NAME 'DEL) X))))))))
                (CAR J))
               (SETQ J (CDR J))
               (GO LAB))
             (RETURN DEL_PRD))))) 
(PUT 'SIMPEPSI_MON_EXPR 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPEPSI_MON_EXPR 'DEFINED-ON-LINE '454) 
(PUT 'SIMPEPSI_MON_EXPR 'DEFINED-IN-FILE 'ASSIST/CONTRTNS.RED) 
(PUT 'SIMPEPSI_MON_EXPR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPEPSI_MON_EXPR (MSF)
    (COND ((OR (ATOM MSF) (ATOM (CAR MSF))) MSF)
          (T
           (PROG (TENS_MSF NOTENS X DEL_PRD)
             (SETQ TENS_MSF (SEP_TENS_FROM_OTHER MSF))
             (SETQ NOTENS (CADR TENS_MSF))
             (SETQ NOTENS
                     (COND (NOTENS NOTENS) (T (COND ((ZEROP 1) NIL) (T 1)))))
             (SETQ TENS_MSF (CAR TENS_MSF))
             (COND ((NULL TENS_MSF) (RETURN MSF)))
             (SETQ X (MK_EPS_LST TENS_MSF))
             (SETQ TENS_MSF (REVERSE (CADR X)))
             (SETQ DEL_PRD (EPSI_TO_DEL (CAR X)))
             (SETQ X
                     (COND
                      (DEL_PRD
                       (COND (*PHYSOP-LOADED (PHYSOP-MULTF DEL_PRD NOTENS))
                             (T (POLY-MULTF DEL_PRD NOTENS))))
                      (T NOTENS)))
             (PROG (J)
               (SETQ J TENS_MSF)
              LAB
               (COND ((NULL J) (RETURN NIL)))
               ((LAMBDA (J)
                  (SETQ X
                          ((LAMBDA (G606)
                             (COND (*PHYSOP-LOADED (PHYSOP-MULTF G606 X))
                                   (T (POLY-MULTF G606 X))))
                           (LIST (CONS (CONS J 1) 1)))))
                (CAR J))
               (SETQ J (CDR J))
               (GO LAB))
             (RETURN X))))) 
(ENDMODULE) 