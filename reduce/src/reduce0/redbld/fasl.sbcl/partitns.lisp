(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PARTITNS)) 
(GLOBAL '(DIMEX* SGN* SIGNAT* SPACES* NUMINDXL* PAIR_ID_NUM*)) 
(FLUID '(DUMMY_ID* G_DVNAMES EPSILON*)) 
(SWITCH (LIST 'EXDELT)) 
(SWITCH (LIST 'ONESPACE)) 
(SETQ *ONESPACE T) 
(FLAG (LIST 'DELTA 'EPSILON 'DEL 'ETA 'METRIC) 'RESERVED) 
(FLAG (LIST 'MAKE_PARTIC_TENS) 'OPFN) 
(PUT 'MAKE_PARTIC_TENS 'NUMBER-OF-ARGS 2) 
(PUT 'MAKE_PARTIC_TENS 'DEFINED-ON-LINE '48) 
(PUT 'MAKE_PARTIC_TENS 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'MAKE_PARTIC_TENS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_PARTIC_TENS (U V)
    (COND
     ((MEMQ V (LIST 'DELTA 'ETA 'EPSILON 'DEL 'METRIC))
      (PROGN
       (COND
        ((OR (GET U 'AVALUE) (GETRTYPE U) (EQ (GETTYPE U) 'PROCEDURE)
             (MEMQ U (LIST 'SIN 'COS 'TAN 'ATAN 'ACOS 'ASIN 'DF 'INT)))
         (RERROR 'CANTENS 5 (LIST U "may not be defined as tensor")))
        ((FLAGP U 'TENSOR)
         (PROGN
          (LPRI (LIST "*** Warning:" U "redefined as particular tensor"))
          (REMPROP U 'KVALUE)
          (REMPROP U 'SIMPFN)
          (REMPROP U 'BLOC_DIAGONAL)
          (REMFLAG (LIST U) 'GENERIC)
          NIL)))
       ((LAMBDA (X)
          (COND ((AND X (MEMQ V (LIST 'DELTA 'ETA 'DEL))) (REM_TENSOR1 X))))
        (GET V 'NAME))
       (MAKE_TENSOR U NIL)
       (PUT U 'PARTIC_TENS
            (COND ((EQUAL V 'DELTA) 'SIMPDELT) ((EQUAL V 'ETA) 'SIMPETA)
                  ((EQUAL V 'EPSILON) 'SIMPEPSI) ((EQUAL V 'DEL) 'SIMPDEL)
                  ((EQUAL V 'METRIC) 'SIMPMETRIC)))
       (COND
        ((AND (NULL *ONESPACE) (EQUAL V 'EPSILON))
         (COND
          (EPSILON*
           (PROGN
            (PUT V 'NAME U)
            (LPRI (LIST "*** Warning:" U "MUST belong to a space"))
            NIL))
          (T NIL))))
       (PUT V 'NAME U)
       (COND
        ((MEMQ V (LIST 'METRIC 'DELTA))
         (PROGN (FLAG (LIST U) 'GENERIC) (MAKE_BLOC_DIAGONAL U))))
       T))
     (T "unknown keyword"))) 
(PUT 'FIND_NAME 'NUMBER-OF-ARGS 1) 
(PUT 'FIND_NAME 'DEFINED-ON-LINE '99) 
(PUT 'FIND_NAME 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'FIND_NAME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FIND_NAME (U)
    ((LAMBDA (X)
       (COND
        ((NULL X) (RERROR 'CANTENS 6 (LIST " no name found for" (LIST U))))
        (T X)))
     (GET U 'NAME))) 
(PUT 'SIMPDELT 'NUMBER-OF-ARGS 2) 
(PUT 'SIMPDELT 'DEFINED-ON-LINE '109) 
(PUT 'SIMPDELT 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'SIMPDELT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMPDELT (X VARL)
    (COND
     ((AND VARL (NULL (CDR X)))
      (LIST (CONS (GETPOWER (FKERN (CONS (CAR X) (CONS VARL NIL))) 1) 1)))
     ((OR (NULL VARL) (NULL (CDR VARL)))
      (PROG (DELT IND Y YV YC)
        (SETQ DELT (CAR X))
        (SETQ IND (CDR X))
        (SETQ Y (SPLIT_COV_CONT_IDS IND))
        (COND
         ((NEQ (TIMES (LENGTH (CAR Y)) (LENGTH (CADR Y))) 1)
          (RERROR 'CANTENS 7 "bad choice of indices for DELTA tensor")))
        (SETQ YV (CAAR Y))
        (SETQ YC (CAADR Y))
        (COND
         ((AND (*ID2NUM YV) (*ID2NUM YC))
          (RETURN (COND ((EQUAL YV YC) 1) (T 0))))
         (*ONESPACE
          (RETURN
           (COND ((EQ YV YC) DIMEX*)
                 (T
                  (LIST
                   (CONS
                    (GETPOWER
                     (FKERN
                      (CONS DELT (APPEND (CADR Y) (LOWERIND_LST (CAR Y)))))
                     1)
                    1))))))
         (T
          (RETURN
           (COND
            ((AND (NULL (GET YV 'SPACE)) (EQ YV YC))
             (COND
              ((ASSOC 'WHOLESPACE SPACES*)
               (LIST
                (CONS (GETPOWER (FKERN (GET_DIM_SPACE 'WHOLESPACE)) 1) 1)))
              (T "not meaningful")))
            ((EQ YV YC)
             (LIST (CONS (GETPOWER (FKERN (SPACE_DIM_OF_IDX YV)) 1) 1)))
            (T
             (LIST
              (CONS
               (GETPOWER
                (FKERN (CONS DELT (APPEND (CADR Y) (LOWERIND_LST (CAR Y))))) 1)
               1)))))))))
     (T "not meaningful"))) 
(PUT 'SIMPDEL 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPDEL 'DEFINED-ON-LINE '147) 
(PUT 'SIMPDEL 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'SIMPDEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPDEL (U)
    (PROG (DEL IND X IDV IDC IDVN IDCN BOOL SPWEIGHT FREE_IND TOT_IND
           DIM_SPACE)
      (SETQ FREE_IND 0)
      (SETQ TOT_IND 0)
      (SETQ DIM_SPACE 0)
      (SETQ DEL (CAR U))
      (SETQ IND (CDR U))
      (SETQ SPWEIGHT 1)
      (SETQ X (SPLIT_COV_CONT_IDS IND))
      (SETQ IDV (CAR X))
      (SETQ IDC (CADR X))
      (COND
       ((NEQ (LENGTH IDV) (LENGTH IDC))
        (RERROR 'CANTENS 7 "bad choice of indices for DEL tensor"))
       ((NULL *ONESPACE)
        (COND
         ((NULL (|SYMB_IDS_BELONG_SAME_SPACE:| (APPEND IDV IDC) NIL))
          (RERROR 'CANTENS 7 "all indices should belong to the SAME space"))
         ((OR (REPEATS IDV) (REPEATS IDC)) (RETURN 0))
         ((EQUAL (LENGTH IDC) 1)
          (RETURN
           (APPLY2 'SIMPDELT
                   (CONS (FIND_NAME 'DELTA) (APPEND (LOWERIND_LST IDV) IDC))
                   NIL))))))
      (SETQ IDV
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y IDV)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Y)
                                    (COND
                                     ((AND (NULL (*ID2NUM Y)) (MEMQ Y IDC))
                                      (LIST 'DUM Y))
                                     (T Y)))
                                  (CAR Y))
                                 NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Y)
                            (COND
                             ((AND (NULL (*ID2NUM Y)) (MEMQ Y IDC))
                              (LIST 'DUM Y))
                             (T Y)))
                          (CAR Y))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ IDC
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y IDC)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Y)
                                    (COND
                                     ((AND (NULL (*ID2NUM Y)) (MEMQ Y (CAR X)))
                                      (LIST 'DUM Y))
                                     (T Y)))
                                  (CAR Y))
                                 NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Y)
                            (COND
                             ((AND (NULL (*ID2NUM Y)) (MEMQ Y (CAR X)))
                              (LIST 'DUM Y))
                             (T Y)))
                          (CAR Y))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((EQUAL (PERMP (SETQ IDVN (ORDN IDV)) IDV)
               (PERMP (SETQ IDCN (ORDN IDC)) IDC))
        (SETQ BOOL T)))
      (COND
       ((NUM_INDLISTP (APPEND IDVN IDCN))
        (RETURN (SIMPDELNUM IDVN IDCN BOOL))))
      (SETQ TOT_IND (LENGTH IDVN))
      (SETQ IDV (|SPLITLIST:| IDVN 'DUM))
      (SETQ FREE_IND (DIFFERENCE TOT_IND (LENGTH IDV)))
      (SETQ DIM_SPACE
              (COND
               (IDV
                (COND ((NULL SPACES*) DIMEX*)
                      (T
                       (LIST
                        (CONS
                         (GETPOWER (FKERN (SPACE_DIM_OF_IDX (CADAR IDV))) 1)
                         1)))))))
      (PROG (I)
        (SETQ I FREE_IND)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE TOT_IND 1) I)) (RETURN NIL)))
        (PROGN
         (SETQ SPWEIGHT
                 ((LAMBDA (G554)
                    (COND (*PHYSOP-LOADED (PHYSOP-MULTF G554 SPWEIGHT))
                          (T (POLY-MULTF G554 SPWEIGHT))))
                  (ADDF DIM_SPACE (NEGF (COND ((ZEROP I) NIL) (T I))))))
         (SETQ IDVN (CDR IDVN))
         (SETQ IDCN (CDR IDCN))
         NIL)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (SETQ SPWEIGHT (*Q2F (SIMP* (REVAL1 (PREPF SPWEIGHT) T))))
      (COND ((NULL IDVN) (RETURN (COND (BOOL SPWEIGHT) (T (NEGF SPWEIGHT))))))
      (COND
       ((NUM_INDLISTP (APPEND IDVN IDCN))
        (RETURN
         ((LAMBDA (G557)
            (COND (*PHYSOP-LOADED (PHYSOP-MULTF SPWEIGHT G557))
                  (T (POLY-MULTF SPWEIGHT G557))))
          (SIMPDELNUM IDVN IDCN BOOL)))))
      (RETURN
       (COND
        (*EXDELT
         (COND
          (BOOL
           ((LAMBDA (G559)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF SPWEIGHT G559))
                    (T (POLY-MULTF SPWEIGHT G559))))
            (EXTRACT_DELT DEL IDVN IDCN 'FULL)))
          (T
           (NEGF
            ((LAMBDA (G561)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF SPWEIGHT G561))
                     (T (POLY-MULTF SPWEIGHT G561))))
             (EXTRACT_DELT DEL IDVN IDCN 'FULL))))))
        ((EQUAL (LENGTH IDVN) 1)
         (COND
          (BOOL
           ((LAMBDA (G563)
              (COND (*PHYSOP-LOADED (PHYSOP-MULTF SPWEIGHT G563))
                    (T (POLY-MULTF SPWEIGHT G563))))
            (LIST
             (CONS
              (GETPOWER
               (FKERN
                (CONS (FIND_NAME 'DELTA) (APPEND (LOWERIND_LST IDVN) IDCN)))
               1)
              1))))
          (T
           (NEGF
            ((LAMBDA (G565)
               (COND (*PHYSOP-LOADED (PHYSOP-MULTF SPWEIGHT G565))
                     (T (POLY-MULTF SPWEIGHT G565))))
             (LIST
              (CONS
               (GETPOWER
                (FKERN
                 (CONS (FIND_NAME 'DELTA) (APPEND (LOWERIND_LST IDVN) IDCN)))
                1)
               1)))))))
        (BOOL
         ((LAMBDA (G567)
            (COND (*PHYSOP-LOADED (PHYSOP-MULTF SPWEIGHT G567))
                  (T (POLY-MULTF SPWEIGHT G567))))
          (LIST
           (CONS
            (GETPOWER (FKERN (CONS DEL (APPEND (LOWERIND_LST IDVN) IDCN))) 1)
            1))))
        (T
         ((LAMBDA (G569)
            (COND (*PHYSOP-LOADED (PHYSOP-MULTF SPWEIGHT G569))
                  (T (POLY-MULTF SPWEIGHT G569))))
          (NEGF
           (LIST
            (CONS
             (GETPOWER (FKERN (CONS DEL (APPEND (LOWERIND_LST IDVN) IDCN))) 1)
             1))))))))) 
(PUT 'SIMPDELNUM 'NUMBER-OF-ARGS 3) 
(PUT 'SIMPDELNUM 'DEFINED-ON-LINE '245) 
(PUT 'SIMPDELNUM 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'SIMPDELNUM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIMPDELNUM (IDVN IDCN BOOL)
    (COND ((EQUAL IDVN IDCN) (COND (BOOL 1) (T (MINUS 1)))) (T 0))) 
(PUT 'EXTRACT_DELT 'NUMBER-OF-ARGS 4) 
(PUT 'EXTRACT_DELT 'DEFINED-ON-LINE '252) 
(PUT 'EXTRACT_DELT 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'EXTRACT_DELT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXTRACT_DELT (DEL IDVN IDCN DEPTH)
    (COND
     ((EQUAL (LENGTH IDCN) 1)
      (APPLY2 (FUNCTION SIMPDELT)
              (CONS (GET 'DELTA 'NAME)
                    (CONS (LIST 'MINUS (CAR IDVN)) (CONS (CAR IDCN) NIL)))
              NIL))
     (T
      (PROG (UU X IND)
        (SETQ IND (CAR IDCN))
        (SETQ IDCN (CDR IDCN))
        (COND
         ((EQUAL DEPTH 1)
          (PROG (I)
            (SETQ I 1)
           LAB
            (COND ((MINUSP (DIFFERENCE (LENGTH IDVN) I)) (RETURN NIL)))
            (PROGN
             (SETQ X
                     ((LAMBDA (G572 G573)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G572 G573))
                              (T (POLY-MULTF G572 G573))))
                      (EXPTF (MINUS 1) (DIFFERENCE I 1))
                      ((LAMBDA (G570 G571)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G570 G571))
                               (T (POLY-MULTF G570 G571))))
                       (APPLY2 (FUNCTION SIMPDELT)
                               (CONS (GET 'DELTA 'NAME)
                                     (CONS IND
                                           (LIST (LIST 'MINUS (NTH IDVN I)))))
                               NIL)
                       (*Q2F
                        (MKSQ
                         (CONS
                          (COND ((EQUAL (LENGTH IDVN) 2) (GET 'DELTA 'NAME))
                                (T DEL))
                          (APPEND IDCN (LOWERIND_LST (REMOVE IDVN I))))
                         1)))))
             (SETQ UU (ADDF X UU)))
            (SETQ I (PLUS2 I 1))
            (GO LAB)))
         ((EQUAL DEPTH 'FULL)
          (PROG (I)
            (SETQ I 1)
           LAB
            (COND ((MINUSP (DIFFERENCE (LENGTH IDVN) I)) (RETURN NIL)))
            (PROGN
             (SETQ X
                     ((LAMBDA (G576 G577)
                        (COND (*PHYSOP-LOADED (PHYSOP-MULTF G576 G577))
                              (T (POLY-MULTF G576 G577))))
                      (EXPTF (MINUS 1) (DIFFERENCE I 1))
                      ((LAMBDA (G574 G575)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF G574 G575))
                               (T (POLY-MULTF G574 G575))))
                       (APPLY2 (FUNCTION SIMPDELT)
                               (CONS (GET 'DELTA 'NAME)
                                     (CONS IND
                                           (LIST (LIST 'MINUS (NTH IDVN I)))))
                               NIL)
                       (EXTRACT_DELT DEL (REMOVE IDVN I) IDCN DEPTH))))
             (SETQ UU (ADDF X UU)))
            (SETQ I (PLUS2 I 1))
            (GO LAB))))
        (RETURN UU))))) 
(PUT 'IDX_NOT_MEMBER_WHOSP 'NUMBER-OF-ARGS 1) 
(PUT 'IDX_NOT_MEMBER_WHOSP 'DEFINED-ON-LINE '290) 
(PUT 'IDX_NOT_MEMBER_WHOSP 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'IDX_NOT_MEMBER_WHOSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDX_NOT_MEMBER_WHOSP (U)
    ((LAMBDA (X) (COND (X (NEQ X 'WHOLESPACE)))) (GET U 'SPACE))) 
(PUT 'IDS_NOT_MEMBER_WHOSP 'NUMBER-OF-ARGS 1) 
(PUT 'IDS_NOT_MEMBER_WHOSP 'DEFINED-ON-LINE '294) 
(PUT 'IDS_NOT_MEMBER_WHOSP 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'IDS_NOT_MEMBER_WHOSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDS_NOT_MEMBER_WHOSP (U)
    (COND ((NULL U) T)
          ((IDX_NOT_MEMBER_WHOSP (CAR U)) (IDS_NOT_MEMBER_WHOSP (CDR U)))
          (T NIL))) 
(PUT 'SIMPETA 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPETA 'DEFINED-ON-LINE '301) 
(PUT 'SIMPETA 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'SIMPETA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPETA (U)
    (COND
     ((AND *ONESPACE (EQUAL SIGNAT* 0))
      (MSGPRI NIL NIL "signature must be defined equal to 1 for ETA tensor" NIL
              T))
     ((AND (NULL *ONESPACE)
           (NULL (GET_SIGN_SPACE (GET (CAR U) 'BELONG_TO_SPACE))))
      (MSGPRI NIL NIL "ETA tensor not properly assigned to a space" NIL NIL))
     (T
      (PROG (ETA IND X)
        (SETQ ETA (CAR U))
        (SETQ IND (CDR U))
        (FLAG (LIST ETA) 'SYMMETRIC)
        (SETQ X (SPLIT_COV_CONT_IDS IND))
        (COND
         ((AND (CAR X) (CADR X))
          (RETURN (APPLY2 'SIMPDELT (CONS (FIND_NAME 'DELTA) IND) NIL))))
        (SETQ X (COND ((NULL (CAR X)) (CADR X)) (T (CAR X))))
        (COND
         ((NEQ (LENGTH X) 2)
          (RERROR 'CANTENS 8 "bad choice of indices for ETA tensor")))
        (SETQ X
                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Y X)
                  (COND ((NULL Y) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (Y) (*ID2NUM Y)) (CAR Y))
                                        NIL)))
                 LOOPLABEL
                  (SETQ Y (CDR Y))
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (Y) (*ID2NUM Y)) (CAR Y)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (RETURN
         (COND ((NUMLIS X) (NUM_ETA X))
               (*ONESPACE
                (LIST (CONS (GETPOWER (FKERN (CONS ETA (ORDN IND))) 1) 1)))
               ((AND (IDS_NOT_MEMBER_WHOSP (LIST (CAR IND) (CADR IND)))
                     (NEQ (GET (CAR IND) 'SPACE) (GET (CADR IND) 'SPACE)))
                0)
               (T
                (LIST
                 (CONS (GETPOWER (FKERN (CONS ETA (ORDN IND))) 1) 1))))))))) 
(PUT 'NUM_ETA 'NUMBER-OF-ARGS 1) 
(PUT 'NUM_ETA 'DEFINED-ON-LINE '336) 
(PUT 'NUM_ETA 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'NUM_ETA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_ETA (U)
    (COND
     ((EQUAL (CAR U) (CADR U)) (COND ((EQUAL (CAR U) 0) SGN*) (T (NEGF SGN*))))
     (T 0))) 
(PUT 'SIMPEPSI 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPEPSI 'DEFINED-ON-LINE '344) 
(PUT 'SIMPEPSI 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'SIMPEPSI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPEPSI (U)
    (PROG (EPSI IND X SPX BOOL)
      (SETQ EPSI (CAR U))
      (SETQ SPX (GET EPSI 'BELONG_TO_SPACE))
      (SETQ IND (CDR U))
      (FLAG (LIST EPSI) 'ANTISYMMETRIC)
      (SETQ X (SPLIT_COV_CONT_IDS IND))
      (COND ((NULL (CAR X)) (SETQ X (CONS 'CONT (CADR X))))
            ((NULL (CADR X)) (SETQ X (CONS 'COV (CAR X))))
            (T (SETQ X (CONS 'MIXED (APPEND (CAR X) (CADR X))))))
      ((LAMBDA (Y)
         (COND
          ((AND (FIXP Y) (NEQ Y (LENGTH (CDR X))))
           (RERROR 'CANTENS 9
                   (LIST "bad number of indices for " (LIST (CAR U))
                         " tensor")))))
       (COND (SPX (GET_DIM_SPACE SPX))
             (T ((LAMBDA (Z) (COND ((FIXP Z) Z))) (WHOLESPACE_DIM '?)))))
      (COND ((REPEATS X) (RETURN 0)))
      (COND
       ((AND (NULL *ONESPACE) SPX)
        (COND
         ((NULL (IND_SAME_SPACE_TENS (CDR U) (CAR U)))
          (RERROR 'CANTENS 9
                  (LIST "some indices are not in the space of" EPSI))))))
      (RETURN
       (COND
        ((OR (EQ (CAR X) 'MIXED) (NOT (NUM_INDLISTP (CDR X))))
         (PROG (XX XY)
           (SETQ XX (ORDN IND))
           (SETQ BOOL (PERMP XX IND))
           (COND
            ((EQ (CAR X) 'MIXED)
             (PROGN
              (SETQ XY (CONT_BEFORE_COV IND))
              (COND ((NULL (PERMP XY XX)) (SETQ BOOL (NOT BOOL)))))))
           (RETURN
            (COND
             (BOOL
              (LIST
               (CONS
                (GETPOWER
                 (FKERN (CONS EPSI (COND ((EQ (CAR X) 'MIXED) XY) (T XX)))) 1)
                1)))
             (T
              (NEGF
               (LIST
                (CONS
                 (GETPOWER
                  (FKERN (CONS EPSI (COND ((EQ (CAR X) 'MIXED) XY) (T XX)))) 1)
                 1))))))))
        ((OR *ONESPACE (NULL SPX))
         (COND ((EQUAL SIGNAT* 0) (NUM_EPSI_EUCLID X))
               ((EQUAL SIGNAT* 1) (NUM_EPSI_NON_EUCLID EPSI X)) (T NIL)))
        ((OR (NULL (GET_SIGN_SPACE SPX)) (EQUAL (GET_SIGN_SPACE SPX) 0))
         (NUM_EPSI_EUCLID (CDR X)))
        ((EQUAL (GET_SIGN_SPACE SPX) 1) (NUM_EPSI_NON_EUCLID EPSI X))
        (T "undetermined signature or signature bigger then 1"))))) 
(PUT 'NUM_EPSI_NON_EUCLID 'NUMBER-OF-ARGS 2) 
(PUT 'NUM_EPSI_NON_EUCLID 'DEFINED-ON-LINE '408) 
(PUT 'NUM_EPSI_NON_EUCLID 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'NUM_EPSI_NON_EUCLID 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NUM_EPSI_NON_EUCLID (EPSI IND)
    (PROG (X)
      (SETQ X (ORDN (CDR IND)))
      (RETURN
       (COND
        ((EQ (CAR IND) 'CONT)
         ((LAMBDA (Y)
            (COND (Y Y)
                  ((PERMP X (CDR IND))
                   (LIST (CONS (GETPOWER (FKERN (CONS EPSI X)) 1) 1)))
                  (T
                   (NEGF (LIST (CONS (GETPOWER (FKERN (CONS EPSI X)) 1) 1))))))
          (*Q2F (MATCH_KVALUE EPSI X NIL))))
        ((EQ (CAR IND) 'COV)
         (COND
          ((EQUAL SGN* 1)
           (COND
            ((EVENP (LENGTH (CDR X)))
             ((LAMBDA (Y)
                (COND (Y Y)
                      ((PERMP X (CDR IND))
                       (LIST (CONS (GETPOWER (FKERN (CONS EPSI X)) 1) 1)))
                      (T
                       (NEGF
                        (LIST (CONS (GETPOWER (FKERN (CONS EPSI X)) 1) 1))))))
              (*Q2F (MATCH_KVALUE EPSI X NIL))))
            (T
             ((LAMBDA (Y)
                (COND (Y (NEGF Y))
                      ((PERMP X (CDR IND))
                       (NEGF
                        (LIST (CONS (GETPOWER (FKERN (CONS EPSI X)) 1) 1))))
                      (T (LIST (CONS (GETPOWER (FKERN (CONS EPSI X)) 1) 1)))))
              (*Q2F (MATCH_KVALUE EPSI X NIL))))))
          ((EQUAL SGN* (MINUS 1))
           ((LAMBDA (Y)
              (COND (Y (NEGF Y))
                    ((PERMP X (CDR IND))
                     (NEGF (LIST (CONS (GETPOWER (FKERN (CONS EPSI X)) 1) 1))))
                    (T (LIST (CONS (GETPOWER (FKERN (CONS EPSI X)) 1) 1)))))
            (*Q2F (MATCH_KVALUE EPSI X NIL))))
          (T NIL)))
        (T NIL))))) 
(FLAG (LIST 'SHOW_EPSILONS) 'OPFN) 
(PUT 'SHOW_EPSILONS 'NUMBER-OF-ARGS 0) 
(PUT 'SHOW_EPSILONS 'DEFINED-ON-LINE '448) 
(PUT 'SHOW_EPSILONS 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'SHOW_EPSILONS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SHOW_EPSILONS NIL
    ((LAMBDA (X)
       (COND ((NULL X) (LIST 'LIST))
             (T
              (CONS 'LIST
                    (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                      (SETQ Y X)
                      (COND ((NULL Y) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (Y)
                                          (LIST 'LIST
                                                (MK*SQ
                                                 (CONS
                                                  (LIST
                                                   (CONS (CONS (CAR Y) 1) 1))
                                                  1))
                                                (MK*SQ
                                                 (CONS
                                                  (LIST
                                                   (CONS (CONS (CDR Y) 1) 1))
                                                  1))))
                                        (CAR Y))
                                       NIL)))
                     LOOPLABEL
                      (SETQ Y (CDR Y))
                      (COND ((NULL Y) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (Y)
                                  (LIST 'LIST
                                        (MK*SQ
                                         (CONS (LIST (CONS (CONS (CAR Y) 1) 1))
                                               1))
                                        (MK*SQ
                                         (CONS (LIST (CONS (CONS (CDR Y) 1) 1))
                                               1))))
                                (CAR Y))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))))
     EPSILON*)) 
(PUT 'MATCH_KVALUE 'NUMBER-OF-ARGS 3) 
(PUT 'MATCH_KVALUE 'DEFINED-ON-LINE '454) 
(PUT 'MATCH_KVALUE 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'MATCH_KVALUE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MATCH_KVALUE (TE IND VARL)
    ((LAMBDA (X) (COND (X (SIMP* (CADR X)))))
     (COND (VARL (ASSOC (CONS TE (CONS VARL IND)) (GET TE 'KVALUE)))
           (T (ASSOC (CONS TE IND) (GET TE 'KVALUE)))))) 
(PUT 'NUM_EPSI_EUCLID 'NUMBER-OF-ARGS 1) 
(PUT 'NUM_EPSI_EUCLID 'DEFINED-ON-LINE '463) 
(PUT 'NUM_EPSI_EUCLID 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'NUM_EPSI_EUCLID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NUM_EPSI_EUCLID (IND)
    (PROG (X)
      (SETQ X (ORDN IND))
      (RETURN (COND ((PERMP X IND) 1) (T (MINUS 1)))))) 
(PUT 'SIMPMETRIC 'NUMBER-OF-ARGS 2) 
(PUT 'SIMPMETRIC 'DEFINED-ON-LINE '473) 
(PUT 'SIMPMETRIC 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'SIMPMETRIC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMPMETRIC (U VAR)
    (PROG (G IND X)
      (COND ((SETQ X (OPMTCH U)) (RETURN (SIMP X))))
      (SETQ G (CAR U))
      (SETQ IND (CDR U))
      (FLAG (LIST G) 'SYMMETRIC)
      (SETQ X (SPLIT_COV_CONT_IDS IND))
      (COND
       ((AND (CAR X) (CADR X))
        (RETURN
         (CONS (APPLY2 'SIMPDELT (CONS (FIND_NAME 'DELTA) IND) NIL) 1))))
      (SETQ X (COND ((NULL (CAR X)) (CADR X)) (T (CAR X))))
      (COND
       ((NEQ (LENGTH X) 2)
        (RERROR 'CANTENS 10 "bad choice of indices for a METRIC tensor")))
      (SETQ X
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y X)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (*ID2NUM Y)) (CAR Y)) NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (*ID2NUM Y)) (CAR Y)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (COND
        ((NUMLIS X)
         (COND
          (*ONESPACE
           (COND ((SETQ X (MATCH_KVALUE G (ORDN IND) VAR)) X)
                 (T
                  (CONS
                   (LIST
                    (CONS
                     (GETPOWER
                      (FKERN
                       (CONS G
                             (COND (VAR (CONS VAR (ORDN IND)))
                                   (T (ORDN IND)))))
                      1)
                     1))
                   1))))
          (T (CONS (MULT_SPACES_NUM_METRIC G IND VAR) 1))))
        (*ONESPACE
         (COND ((SETQ X (MATCH_KVALUE G (ORDN IND) VAR)) X)
               (T
                (CONS
                 (LIST
                  (CONS
                   (GETPOWER
                    (FKERN
                     (CONS G
                           (COND (VAR (CONS VAR (ORDN IND))) (T (ORDN IND)))))
                    1)
                   1))
                 1))))
        ((NEQ (GET (CAR IND) 'SPACE) (GET (CADR IND) 'SPACE)) 0)
        ((SETQ X (MATCH_KVALUE G (ORDN IND) VAR)) X)
        (T
         (CONS
          (LIST
           (CONS
            (GETPOWER
             (FKERN (CONS G (COND (VAR (CONS VAR (ORDN IND))) (T (ORDN IND)))))
             1)
            1))
          1)))))) 
(PUT 'MULT_SPACES_NUM_METRIC 'NUMBER-OF-ARGS 3) 
(PUT 'MULT_SPACES_NUM_METRIC 'DEFINED-ON-LINE '513) 
(PUT 'MULT_SPACES_NUM_METRIC 'DEFINED-IN-FILE 'ASSIST/PARTITNS.RED) 
(PUT 'MULT_SPACES_NUM_METRIC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULT_SPACES_NUM_METRIC (G IND VAR)
    (PROG (X Y)
      (SETQ X (COND ((PAIRP (CAR IND)) (RAISEIND_LST IND)) (T IND)))
      (RETURN
       (COND
        ((AND NUMINDXL* (NULL (NUMIDS2_BELONG_SAME_SPACE (CAR X) (CADR X) G)))
         0)
        ((SETQ Y
                 (MATCH_KVALUE G
                  (COND (VAR (CONS VAR (ORDN IND))) (T (ORDN IND))) VAR))
         Y)
        (T
         (LIST
          (CONS
           (GETPOWER
            (FKERN (CONS G (COND (VAR (CONS VAR (ORDN IND))) (T (ORDN IND)))))
            1)
           1))))))) 
(ENDMODULE) 