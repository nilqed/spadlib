(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'AUXITENS)) 
(REMFLAG (LIST 'MINUS) 'INTFN) 
(GLOBAL '(DIMEX* SGN* SIGNAT* SPACES* NUMINDXL* PAIR_ID_NUM*)) 
(SETQ PAIR_ID_NUM*
        '((|0| . 0) (|1| . 1) (|2| . 2) (|3| . 3) (|4| . 4) (|5| . 5) (|6| . 6)
          (|7| . 7) (|8| . 8) (|9| . 9) (|10| . 10) (|11| . 11) (|12| . 12)
          (|13| . 13))) 
(FLUID '(DUMMY_ID* G_DVNAMES EPSILON*)) 
(SWITCH (LIST 'ONESPACE)) 
(SETQ *ONESPACE T) 
(PUT '|RAISEIND:| 'NUMBER-OF-ARGS 1) 
(PUT '|RAISEIND:| 'DEFINED-ON-LINE '50) 
(PUT '|RAISEIND:| 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT '|RAISEIND:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |RAISEIND:| (U) (COND ((ATOM U) U) (T (CADR U)))) 
(PUT 'LOWERIND_LST 'NUMBER-OF-ARGS 1) 
(PUT 'LOWERIND_LST 'DEFINED-ON-LINE '53) 
(PUT 'LOWERIND_LST 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'LOWERIND_LST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LOWERIND_LST (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J)) NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'RAISEIND_LST 'NUMBER-OF-ARGS 1) 
(PUT 'RAISEIND_LST 'DEFINED-ON-LINE '58) 
(PUT 'RAISEIND_LST 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'RAISEIND_LST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RAISEIND_LST (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (J) (|RAISEIND:| J)) (CAR J)) NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) (|RAISEIND:| J)) (CAR J)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'FLATINDXL 'NUMBER-OF-ARGS 1) 
(PUT 'FLATINDXL 'DEFINED-ON-LINE '63) 
(PUT 'FLATINDXL 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'FLATINDXL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FLATINDXL (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (J)
                          (COND ((ATOM J) J) ((EQCAR J 'MINUS) (CADR J))
                                (T (CDR J))))
                        (CAR J))
                       NIL)))
     LOOPLABEL
      (SETQ J (CDR J))
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (J)
                  (COND ((ATOM J) J) ((EQCAR J 'MINUS) (CADR J)) (T (CDR J))))
                (CAR J))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'COV_LST_IDSP 'NUMBER-OF-ARGS 1) 
(PUT 'COV_LST_IDSP 'DEFINED-ON-LINE '72) 
(PUT 'COV_LST_IDSP 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'COV_LST_IDSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COV_LST_IDSP (U)
    (COND ((NULL U) T) ((EQCAR (CAR U) 'MINUS) (COV_LST_IDSP (CDR U))))) 
(PUT 'CONT_LST_IDSP 'NUMBER-OF-ARGS 1) 
(PUT 'CONT_LST_IDSP 'DEFINED-ON-LINE '78) 
(PUT 'CONT_LST_IDSP 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'CONT_LST_IDSP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONT_LST_IDSP (U)
    (COND ((NULL U) T) ((ATOM (CAR U)) (CONT_LST_IDSP (CDR U))))) 
(PUT 'IDENTIFY_POS_COV_LST 'NUMBER-OF-ARGS 2) 
(PUT 'IDENTIFY_POS_COV_LST 'DEFINED-ON-LINE '84) 
(PUT 'IDENTIFY_POS_COV_LST 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'IDENTIFY_POS_COV_LST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDENTIFY_POS_COV_LST (U I)
    (COND ((NULL U) (COND ((EQUAL I 0) NIL) (T (DIFFERENCE I 1))))
          ((COV_LST_IDSP (CAR U)) (SETQ I (PLUS I 1)))
          (T (IDENTIFY_POS_COV_LST (CDR U) (PLUS I 1))))) 
(PUT 'IDENTIFY_POS_CONT_LST 'NUMBER-OF-ARGS 2) 
(PUT 'IDENTIFY_POS_CONT_LST 'DEFINED-ON-LINE '97) 
(PUT 'IDENTIFY_POS_CONT_LST 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'IDENTIFY_POS_CONT_LST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IDENTIFY_POS_CONT_LST (U I)
    (COND ((NULL U) (COND ((EQUAL I 0) NIL) (T (DIFFERENCE I 1))))
          ((CONT_LST_IDSP (CAR U)) (SETQ I (PLUS I 1)))
          (T (IDENTIFY_POS_CONT_LST (CDR U) (PLUS I 1))))) 
(PUT '|SPLITLIST:| 'NUMBER-OF-ARGS 2) 
(PUT '|SPLITLIST:| 'DEFINED-ON-LINE '110) 
(PUT '|SPLITLIST:| 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT '|SPLITLIST:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SPLITLIST:| (U IDP)
    (COND ((NULL U) NIL)
          ((EQCAR (CAR U) IDP) (CONS (CAR U) (|SPLITLIST:| (CDR U) IDP)))
          (T (|SPLITLIST:| (CDR U) IDP)))) 
(PUT '|LIST_TO_IDS:| 'NUMBER-OF-ARGS 1) 
(PUT '|LIST_TO_IDS:| 'DEFINED-ON-LINE '119) 
(PUT '|LIST_TO_IDS:| 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT '|LIST_TO_IDS:| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |LIST_TO_IDS:| (L)
    (COND ((ATOM L) (REDERR "argument for list_to_ids must be a list"))
          (T
           (INTERN
            (COMPRESS
             (PROG (I FORALL-RESULT FORALL-ENDPTR)
               (SETQ I L)
              STARTOVER
               (COND ((NULL I) (RETURN NIL)))
               (SETQ FORALL-RESULT ((LAMBDA (I) (EXPLODE I)) (CAR I)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ I (CDR I))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL I) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR ((LAMBDA (I) (EXPLODE I)) (CAR I)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ I (CDR I))
               (GO LOOPLABEL))))))) 
(PUT '|SPLIT:| 'NUMBER-OF-ARGS 2) 
(PUT '|SPLIT:| 'DEFINED-ON-LINE '125) 
(PUT '|SPLIT:| 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT '|SPLIT:| 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE |SPLIT:| (U V)
    (COND
     ((AND (LISTP U) (LISTP V))
      (PROG (X)
        (RETURN
         (PROG (N FORALL-RESULT FORALL-ENDPTR)
           (SETQ N V)
           (COND ((NULL N) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (N)
                               (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ I 1)
                                 (COND
                                  ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  (PROGN
                                                   (SETQ X (CAR U))
                                                   (SETQ U (REST U))
                                                   X)
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ I (PLUS2 I 1))
                                 (COND
                                  ((MINUSP (DIFFERENCE N I))
                                   (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS
                                          (PROGN
                                           (SETQ X (CAR U))
                                           (SETQ U (REST U))
                                           X)
                                          NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL)))
                             (CAR N))
                            NIL)))
          LOOPLABEL
           (SETQ N (CDR N))
           (COND ((NULL N) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS
                    ((LAMBDA (N)
                       (PROG (I FORALL-RESULT FORALL-ENDPTR)
                         (SETQ I 1)
                         (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          (PROGN
                                           (SETQ X (CAR U))
                                           (SETQ U (REST U))
                                           X)
                                          NIL)))
                        LOOPLABEL
                         (SETQ I (PLUS2 I 1))
                         (COND
                          ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  (PROGN (SETQ X (CAR U)) (SETQ U (REST U)) X)
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))
                     (CAR N))
                    NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL))))))) 
(PUT 'SYMTREE_SPLITLST 'NUMBER-OF-ARGS 3) 
(PUT 'SYMTREE_SPLITLST 'DEFINED-ON-LINE '136) 
(PUT 'SYMTREE_SPLITLST 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'SYMTREE_SPLITLST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SYMTREE_SPLITLST (IDTENS LSY BOOL)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I LSY)
      (COND ((NULL I) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (I)
                          (COND
                           ((AND BOOL (MEMQ (CAR I) (LIST '+ '-)))
                            (ORDN
                             (PROG (J FORALL-RESULT FORALL-ENDPTR)
                               (SETQ J (CDR I))
                               (COND ((NULL J) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (J) (NTH IDTENS J))
                                                 (CAR J))
                                                NIL)))
                              LOOPLABEL
                               (SETQ J (CDR J))
                               (COND ((NULL J) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (J) (NTH IDTENS J)) (CAR J))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))))
                           (T
                            (PROG (J FORALL-RESULT FORALL-ENDPTR)
                              (SETQ J (CDR I))
                              (COND ((NULL J) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (J) (NTH IDTENS J))
                                                (CAR J))
                                               NIL)))
                             LOOPLABEL
                              (SETQ J (CDR J))
                              (COND ((NULL J) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J) (NTH IDTENS J)) (CAR J))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))))
                        (CAR I))
                       NIL)))
     LOOPLABEL
      (SETQ I (CDR I))
      (COND ((NULL I) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (I)
                  (COND
                   ((AND BOOL (MEMQ (CAR I) (LIST '+ '-)))
                    (ORDN
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J (CDR I))
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (J) (NTH IDTENS J)) (CAR J))
                                        NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (NTH IDTENS J)) (CAR J))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))
                   (T
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J (CDR I))
                      (COND ((NULL J) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (J) (NTH IDTENS J)) (CAR J))
                                       NIL)))
                     LOOPLABEL
                      (SETQ J (CDR J))
                      (COND ((NULL J) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (J) (NTH IDTENS J)) (CAR J)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))
                (CAR I))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'SYMTREE_ZEROP 'NUMBER-OF-ARGS 2) 
(PUT 'SYMTREE_ZEROP 'DEFINED-ON-LINE '147) 
(PUT 'SYMTREE_ZEROP 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'SYMTREE_ZEROP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYMTREE_ZEROP (IDTENS LSYM)
    (COND ((NULL (CDR LSYM)) NIL)
          ((NUMLIS (CDR LSYM))
           (COND ((AND (EQ (CAR LSYM) '-) (REPEATS IDTENS)) (REPEATS IDTENS))
                 (T NIL)))
          (T
           (PROG (LSY IDT Y)
             (COND
              ((EQ (CAR LSYM) '-)
               (COND
                ((SETQ Y (REPEATS (SYMTREE_SPLITLST IDTENS (CDR LSYM) NIL)))
                 (RETURN Y)))))
             (SETQ IDT (SYMTREE_SPLITLST IDTENS (CDR LSYM) T))
             (COND
              ((EQ (CAR LSYM) '-) (COND ((SETQ Y (REPEATS IDT)) (RETURN Y)))))
             (SETQ LSY
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J (CDR LSYM))
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (J) (CAR J)) (CAR J))
                                             NIL)))
                      LOOPLABEL
                       (SETQ J (CDR J))
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (RETURN (PARTSYM_ZEROP IDT LSY)))))) 
(PUT 'PARTSYM_ZEROP 'NUMBER-OF-ARGS 2) 
(PUT 'PARTSYM_ZEROP 'DEFINED-ON-LINE '174) 
(PUT 'PARTSYM_ZEROP 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'PARTSYM_ZEROP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PARTSYM_ZEROP (IDT LSY)
    (COND ((NULL IDT) NIL)
          (T
           ((LAMBDA (Y)
              (COND ((AND (EQ (CAR LSY) '-) Y) Y)
                    (T (PARTSYM_ZEROP (CDR IDT) (CDR LSY)))))
            (REPEATS (CAR IDT)))))) 
(PUT 'CONT_BEFORE_COV 'NUMBER-OF-ARGS 1) 
(PUT 'CONT_BEFORE_COV 'DEFINED-ON-LINE '184) 
(PUT 'CONT_BEFORE_COV 'DEFINED-IN-FILE 'ASSIST/AUXITENS.RED) 
(PUT 'CONT_BEFORE_COV 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONT_BEFORE_COV (U)
    (PROG (X)
      (SETQ X (|SPLITLIST:| U 'MINUS))
      (RETURN (APPEND (SETDIFF U X) X)))) 
(ENDMODULE) 