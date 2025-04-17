(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ODIM)) 
(PUT 'DIMZEROP 'PSOPFN 'ODIM=ZEROP) 
(PUT 'ODIM=ZEROP 'NUMBER-OF-ARGS 1) 
(PUT 'ODIM=ZEROP 'DEFINED-ON-LINE '47) 
(PUT 'ODIM=ZEROP 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM=ZEROP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODIM=ZEROP (M)
    (PROG (C)
      (INTF_TEST M)
      (INTF_GET (SETQ M (CAR M)))
      (COND
       ((NOT (SETQ C (GET M 'GBASIS)))
        (PUT M 'GBASIS (SETQ C (GBASIS* (GET M 'BASIS))))))
      (COND ((DIMZEROP* C) (RETURN 'YES)) (T (RETURN 'NO))))) 
(PUT 'DIMZEROP* 'NUMBER-OF-ARGS 1) 
(PUT 'DIMZEROP* 'DEFINED-ON-LINE '55) 
(PUT 'DIMZEROP* 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'DIMZEROP* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIMZEROP* (M) (NULL (ODIM_PARAMETER M))) 
(PUT 'ODIM_PARAMETER 'NUMBER-OF-ARGS 1) 
(PUT 'ODIM_PARAMETER 'DEFINED-ON-LINE '57) 
(PUT 'ODIM_PARAMETER 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM_PARAMETER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODIM_PARAMETER (M) (ODIM=PARAMETER (MOID_FROM_DPMAT M))) 
(PUT 'ODIM=PARAMETER 'NUMBER-OF-ARGS 1) 
(PUT 'ODIM=PARAMETER 'DEFINED-ON-LINE '62) 
(PUT 'ODIM=PARAMETER 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM=PARAMETER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODIM=PARAMETER (M)
    (COND ((NULL M) NIL)
          (T (OR (ODIM=PARAMETER1 (CDAR M)) (ODIM=PARAMETER (CDR M)))))) 
(PUT 'ODIM=PARAMETER1 'NUMBER-OF-ARGS 1) 
(PUT 'ODIM=PARAMETER1 'DEFINED-ON-LINE '66) 
(PUT 'ODIM=PARAMETER1 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM=PARAMETER1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODIM=PARAMETER1 (M)
    (COND
     ((NULL M)
      ((LAMBDA (U) (COND (U (CAR U)) (T U)))
       (REVERSE (RING_NAMES CALI=BASERING))))
     ((MO=ZERO (CAR (CAR M))) NIL)
     (T
      (PROG (B U)
        (SETQ U
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X M)
                 STARTOVER
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          ((LAMBDA (X)
                             (COND
                              ((EQUAL (LENGTH (SETQ B (MO_SUPPORT X))) 1) B)))
                           (CAR X)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                  (SETQ X (CDR X))
                  (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                 LOOPLABEL
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          ((LAMBDA (X)
                             (COND
                              ((EQUAL (LENGTH (SETQ B (MO_SUPPORT X))) 1) B)))
                           (CAR X)))
                  (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                  (SETQ X (CDR X))
                  (GO LOOPLABEL)))
        (SETQ B (REVERSE (RING_NAMES CALI=BASERING)))
        (PROG ()
         WHILELABEL
          (COND ((NOT (AND B (MEMBER (CAR B) U))) (RETURN NIL)))
          (SETQ B (CDR B))
          (GO WHILELABEL))
        (RETURN (COND (B (CAR B)) (T NIL))))))) 
(PUT 'GETKBASE 'PSOPFN 'ODIM=EVKBASE) 
(PUT 'ODIM=EVKBASE 'NUMBER-OF-ARGS 1) 
(PUT 'ODIM=EVKBASE 'DEFINED-ON-LINE '82) 
(PUT 'ODIM=EVKBASE 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM=EVKBASE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODIM=EVKBASE (M)
    (PROG (C)
      (INTF_TEST M)
      (INTF_GET (SETQ M (CAR M)))
      (COND
       ((NOT (SETQ C (GET M 'GBASIS)))
        (PUT M 'GBASIS (SETQ C (GBASIS* (GET M 'BASIS))))))
      (RETURN (MOID_2A (GETKBASE* C))))) 
(PUT 'GETKBASE* 'NUMBER-OF-ARGS 1) 
(PUT 'GETKBASE* 'DEFINED-ON-LINE '90) 
(PUT 'GETKBASE* 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'GETKBASE* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GETKBASE* (M)
    (COND ((NOT (DIMZEROP* M)) (REDERR "dpmat not zerodimensional"))
          (T
           (PROG (U FORALL-RESULT FORALL-ENDPTR)
             (SETQ U (MOID_FROM_DPMAT M))
            STARTOVER
             (COND ((NULL U) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (U)
                        (ODIM=KBASE (MO_FROM_EI (CAR U))
                         (RING_NAMES CALI=BASERING) (CDR U)))
                      (CAR U)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ U (CDR U))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL U) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (U)
                        (ODIM=KBASE (MO_FROM_EI (CAR U))
                         (RING_NAMES CALI=BASERING) (CDR U)))
                      (CAR U)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ U (CDR U))
             (GO LOOPLABEL))))) 
(PUT 'ODIM=KBASE 'NUMBER-OF-ARGS 3) 
(PUT 'ODIM=KBASE 'DEFINED-ON-LINE '95) 
(PUT 'ODIM=KBASE 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM=KBASE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ODIM=KBASE (MO N M)
    (COND ((MOID_MEMBER MO M) NIL)
          (T
           (CONS MO
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X N)
                  STARTOVER
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (ODIM=KBASE (MO_INC MO (CAR X) 1) (APPEND X NIL) M))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ X (CDR X))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (ODIM=KBASE (MO_INC MO (CAR X) 1) (APPEND X NIL) M))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ X (CDR X))
                   (GO LOOPLABEL)))))) 
(PUT 'ODIM_UP 'NUMBER-OF-ARGS 2) 
(PUT 'ODIM_UP 'DEFINED-ON-LINE '102) 
(PUT 'ODIM_UP 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM_UP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODIM_UP (A M)
    (COND
     ((OR (GREATERP (DPMAT_COLS M) 0) (NOT (DIMZEROP* M)))
      (REDERR "univariate polynomials only for zerodimensional ideals"))
     ((NOT (MEMBER A (RING_NAMES CALI=BASERING))) (TYPERR A "variable name"))
     ((DPMAT_UNITIDEAL? M) (DP_FI 1))
     (T
      (PROG (B V P L Q R)
        (SETQ P (CONS (DP_FI 1) (DP_FI 1)))
        (SETQ B (DPMAT_LIST M))
        (SETQ V (MO_FROM_A A))
        (PROG ()
         WHILELABEL
          (COND ((NOT (CDR P)) (RETURN NIL)))
          (PROGN
           (SETQ L (MERGE (LIST P) L (FUNCTION ODIM=GREATER)))
           (SETQ Q (DP_TIMES_MO V (CAR P)))
           (SETQ R (RED_REDPOL B (BAS_MAKE 0 (DP_TIMES_MO V (CDR P)))))
           (SETQ P
                   (ODIM=REDUCE (CONS (DP_PROD (CDR R) Q) (BAS_DPOLY (CAR R)))
                    L))
           NIL)
          (GO WHILELABEL))
        (RETURN (COND (*BCSIMP (CAR (DP_SIMP (CAR P)))) (T (CAR P)))))))) 
(PUT 'ODIM=GREATER 'NUMBER-OF-ARGS 2) 
(PUT 'ODIM=GREATER 'DEFINED-ON-LINE '125) 
(PUT 'ODIM=GREATER 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM=GREATER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODIM=GREATER (A B)
    (EQUAL (MO_COMPARE (DP_LMON (CDR A)) (DP_LMON (CDR B))) 1)) 
(PUT 'ODIM=REDUCE 'NUMBER-OF-ARGS 2) 
(PUT 'ODIM=REDUCE 'DEFINED-ON-LINE '128) 
(PUT 'ODIM=REDUCE 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM=REDUCE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODIM=REDUCE (A L)
    (COND ((OR (NULL (CDR A)) (NULL L) (ODIM=GREATER A (CAR L))) A)
          ((MO_EQUAL? (DP_LMON (CDR A)) (DP_LMON (CDAR L)))
           (PROG (Z Z1 Z2 B)
             (SETQ B (CAR L))
             (SETQ Z1 (CALI_BC_NEG (DP_LC (CDR A))))
             (SETQ Z2 (DP_LC (CDR B)))
             (COND
              (*BCSIMP
               (PROGN
                (COND
                 ((SETQ Z (CALI_BC_INV Z1))
                  (PROGN
                   (SETQ Z1 (CALI_BC_FI 1))
                   (SETQ Z2 (CALI_BC_PROD Z2 Z))))
                 (T
                  (PROGN
                   (SETQ Z (CALI_BC_GCD Z1 Z2))
                   (SETQ Z1 (CAR (CALI_BC_DIVMOD Z1 Z)))
                   (SETQ Z2 (CAR (CALI_BC_DIVMOD Z2 Z)))
                   NIL)))
                NIL)))
             (SETQ A
                     (CONS
                      (DP_SUM (DP_TIMES_BC Z2 (CAR A))
                       (DP_TIMES_BC Z1 (CAR B)))
                      (DP_SUM (DP_TIMES_BC Z2 (CDR A))
                       (DP_TIMES_BC Z1 (CDR B)))))
             (RETURN (ODIM=REDUCE A (CDR L)))))
          (T (ODIM=REDUCE A (CDR L))))) 
(PUT 'ODIM_BORDERBASIS 'NUMBER-OF-ARGS 1) 
(PUT 'ODIM_BORDERBASIS 'DEFINED-ON-LINE '149) 
(PUT 'ODIM_BORDERBASIS 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM_BORDERBASIS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ODIM_BORDERBASIS (M)
    (COND
     ((NOT *NOETHERIAN)
      (REDERR "BORDERBASIS only for non noetherian term orders"))
     ((NOT (DIMZEROP* M))
      (REDERR "BORDERBASIS only for zerodimensional ideals or modules"))
     (T
      (PROG (B V U MO BAS)
        (SETQ BAS (BAS_ZERODELETE (DPMAT_LIST M)))
        (SETQ MO
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X BAS)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (DP_LMON (BAS_DPOLY X)))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (DP_LMON (BAS_DPOLY X))) (CAR X))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ V
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (RING_NAMES CALI=BASERING))
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (X) (MO_FROM_A X)) (CAR X))
                                        NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (MO_FROM_A X)) (CAR X)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ U
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X BAS)
                  (COND ((NULL X) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X)
                                      (LIST (DP_LMON (BAS_DPOLY X))
                                            (RED_TAILRED BAS X)))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (X)
                              (LIST (DP_LMON (BAS_DPOLY X))
                                    (RED_TAILRED BAS X)))
                            (CAR X))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (PROG ()
         WHILELABEL
          (COND ((NOT U) (RETURN NIL)))
          (PROGN
           (SETQ B (APPEND B U))
           (SETQ U
                   (LISTMINIMIZE
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X U)
                     STARTOVER
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (X)
                                 (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ Y V)
                                  STARTOVER
                                   (COND ((NULL Y) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           ((LAMBDA (Y)
                                              (PROG (W)
                                                (SETQ W (MO_SUM (FIRST X) Y))
                                                (COND
                                                 ((AND
                                                   (NOT
                                                    (LISTTEST B W
                                                              (FUNCTION
                                                               (LAMBDA (X Y)
                                                                 (EQUAL (CAR X)
                                                                        Y)))))
                                                   (NOT (ODIM=INTERIOR W MO)))
                                                  (RETURN
                                                   (LIST
                                                    (LIST W Y
                                                          (BAS_DPOLY
                                                           (SECOND X)))))))))
                                            (CAR Y)))
                                   (SETQ FORALL-ENDPTR
                                           (LASTPAIR FORALL-RESULT))
                                   (SETQ Y (CDR Y))
                                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                  LOOPLABEL
                                   (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           ((LAMBDA (Y)
                                              (PROG (W)
                                                (SETQ W (MO_SUM (FIRST X) Y))
                                                (COND
                                                 ((AND
                                                   (NOT
                                                    (LISTTEST B W
                                                              (FUNCTION
                                                               (LAMBDA (X Y)
                                                                 (EQUAL (CAR X)
                                                                        Y)))))
                                                   (NOT (ODIM=INTERIOR W MO)))
                                                  (RETURN
                                                   (LIST
                                                    (LIST W Y
                                                          (BAS_DPOLY
                                                           (SECOND X)))))))))
                                            (CAR Y)))
                                   (SETQ FORALL-ENDPTR
                                           (LASTPAIR FORALL-ENDPTR))
                                   (SETQ Y (CDR Y))
                                   (GO LOOPLABEL)))
                               (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ X (CDR X))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (X)
                                 (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ Y V)
                                  STARTOVER
                                   (COND ((NULL Y) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           ((LAMBDA (Y)
                                              (PROG (W)
                                                (SETQ W (MO_SUM (FIRST X) Y))
                                                (COND
                                                 ((AND
                                                   (NOT
                                                    (LISTTEST B W
                                                              (FUNCTION
                                                               (LAMBDA (X Y)
                                                                 (EQUAL (CAR X)
                                                                        Y)))))
                                                   (NOT (ODIM=INTERIOR W MO)))
                                                  (RETURN
                                                   (LIST
                                                    (LIST W Y
                                                          (BAS_DPOLY
                                                           (SECOND X)))))))))
                                            (CAR Y)))
                                   (SETQ FORALL-ENDPTR
                                           (LASTPAIR FORALL-RESULT))
                                   (SETQ Y (CDR Y))
                                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                  LOOPLABEL
                                   (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           ((LAMBDA (Y)
                                              (PROG (W)
                                                (SETQ W (MO_SUM (FIRST X) Y))
                                                (COND
                                                 ((AND
                                                   (NOT
                                                    (LISTTEST B W
                                                              (FUNCTION
                                                               (LAMBDA (X Y)
                                                                 (EQUAL (CAR X)
                                                                        Y)))))
                                                   (NOT (ODIM=INTERIOR W MO)))
                                                  (RETURN
                                                   (LIST
                                                    (LIST W Y
                                                          (BAS_DPOLY
                                                           (SECOND X)))))))))
                                            (CAR Y)))
                                   (SETQ FORALL-ENDPTR
                                           (LASTPAIR FORALL-ENDPTR))
                                   (SETQ Y (CDR Y))
                                   (GO LOOPLABEL)))
                               (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ X (CDR X))
                      (GO LOOPLABEL))
                    (FUNCTION (LAMBDA (X Y) (EQUAL (CAR X) (CAR Y))))))
           (SETQ U
                   (PROG (X FORALL-RESULT FORALL-ENDPTR)
                     (SETQ X U)
                     (COND ((NULL X) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (X)
                                         (LIST (FIRST X)
                                               (RED_TAILRED BAS
                                                (BAS_MAKE 0
                                                 (DP_TIMES_MO (SECOND X)
                                                  (THIRD X))))))
                                       (CAR X))
                                      NIL)))
                    LOOPLABEL
                     (SETQ X (CDR X))
                     (COND ((NULL X) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (LIST (FIRST X)
                                       (RED_TAILRED BAS
                                        (BAS_MAKE 0
                                         (DP_TIMES_MO (SECOND X) (THIRD X))))))
                               (CAR X))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           NIL)
          (GO WHILELABEL))
        (RETURN
         (BAS_RENUMBER
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X B)
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (X) (SECOND X)) (CAR X)) NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (SECOND X)) (CAR X)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))))))) 
(PUT 'ODIM=INTERIOR 'NUMBER-OF-ARGS 2) 
(PUT 'ODIM=INTERIOR 'DEFINED-ON-LINE '180) 
(PUT 'ODIM=INTERIOR 'DEFINED-IN-FILE 'CALI/ODIM.RED) 
(PUT 'ODIM=INTERIOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ODIM=INTERIOR (M MO)
    (PROG (B)
      (SETQ B T)
      (PROG (X)
        (SETQ X (MO_SUPPORT M))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ B (AND B (MOID_MEMBER (MO_DIFF M (MO_FROM_A X)) MO))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN B))) 
(ENDMODULE) 