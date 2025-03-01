(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TENSOR1)) 
(GLOBAL '(*BASIS)) 
(GLOBAL '(PV_DEN)) 
(PUT 'TH2PE 'NUMBER-OF-ARGS 2) 
(PUT 'TH2PE 'DEFINED-ON-LINE '42) 
(PUT 'TH2PE 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'TH2PE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TH2PE (TH V)
    (PROG (PE R I IL TT TT1)
      (PROG ()
       WHILELABEL
        (COND ((NOT V) (RETURN NIL)))
        (PROGN
         (SETQ IL (PAPPL (CDAR V) (DI_RESTORE (CADR TH))))
         (SETQ TT1 NIL)
         (PROG (X)
           (SETQ X (CAR TH))
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (PROGN
               (SETQ R (GET X '|:TENSOR|))
               (SETQ TT (LIST X))
               (PROG (I)
                 (SETQ I 1)
                LAB
                 (COND ((MINUSP (DIFFERENCE R I)) (RETURN NIL)))
                 (PROGN (SETQ TT (CONS (CAR IL) TT)) (SETQ IL (CDR IL)))
                 (SETQ I (PLUS2 I 1))
                 (GO LAB))
               (SETQ TT1 (CONS (REVERSIP TT) TT1))
               NIL))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB))
         (SETQ TT1 (REVERSIP TT1))
         (COND ((NULL (EQUAL (CAAR V) 1)) (SETQ TT1 (CONS (CAAR V) TT1))))
         (COND ((AND TT1 (CDR TT1)) (SETQ TT1 (CONS 'TIMES TT1))))
         (COND ((AND TT1 (NULL (CDR TT1))) (SETQ TT1 (CAR TT1))))
         (SETQ PE (CONS TT1 PE))
         (SETQ V (CDR V))
         NIL)
        (GO WHILELABEL))
      (SETQ PE (REVERSIP PE))
      (COND ((AND PE (CDR PE)) (SETQ PE (CONS 'PLUS PE)))
            (PE (SETQ PE (CAR PE))))
      (RETURN PE))) 
(PUT 'T_PRI1 'NUMBER-OF-ARGS 2) 
(PUT 'T_PRI1 'DEFINED-ON-LINE '71) 
(PUT 'T_PRI1 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_PRI1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_PRI1 (TT SW)
    (PROG (PE DEN)
      (SETQ TT (CDR TT))
      (SETQ DEN (CDDR (CAAR TT)))
      (PROG ()
       WHILELABEL
        (COND ((NOT TT) (RETURN NIL)))
        (PROGN
         (SETQ PE (CONS (TH2PE (CAAR TT) (CDAR TT)) PE))
         (SETQ TT (CDR TT))
         NIL)
        (GO WHILELABEL))
      (COND ((AND PE (CDR PE)) (SETQ PE (CONS 'PLUS (REVERSIP PE))))
            (PE (SETQ PE (CAR PE))))
      (COND
       ((NOT (EQUAL DEN 1)) (SETQ PE (CONS 'QUOTIENT (CONS PE (LIST DEN))))))
      (ASSGNPRI PE NIL SW))) 
(PUT 'PAPPL_T 'NUMBER-OF-ARGS 2) 
(PUT 'PAPPL_T 'DEFINED-ON-LINE '88) 
(PUT 'PAPPL_T 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'PAPPL_T 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PAPPL_T (P TT)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X TT)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (X)
                          (CONS
                           (CONS (CAAR X) (CONS (PAPPL P (CADAR X)) (CDDAR X)))
                           (PAPPL_PV P (CDR X))))
                        (CAR X))
                       NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (X)
                  (CONS (CONS (CAAR X) (CONS (PAPPL P (CADAR X)) (CDDAR X)))
                        (PAPPL_PV P (CDR X))))
                (CAR X))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'T_ADD 'NUMBER-OF-ARGS 2) 
(PUT 'T_ADD 'DEFINED-ON-LINE '92) 
(PUT 'T_ADD 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_ADD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_ADD (T1 T2)
    (COND ((NULL (CDR T1)) T2) ((NULL (CDR T2)) T1)
          ((TH_MATCH (CADR T1) (CADR T2)) (SIEVE_T (T_ADD2 T1 T2) *BASIS))
          (T (T_ADDF T1 T2)))) 
(PUT 'SIEVE_T 'NUMBER-OF-ARGS 2) 
(PUT 'SIEVE_T 'DEFINED-ON-LINE '99) 
(PUT 'SIEVE_T 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'SIEVE_T 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIEVE_T (TT BS) (CONS (CAR TT) (SIEVE_T0 (CDR TT) NIL BS))) 
(PUT 'SIEVE_T0 'NUMBER-OF-ARGS 3) 
(PUT 'SIEVE_T0 'DEFINED-ON-LINE '105) 
(PUT 'SIEVE_T0 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'SIEVE_T0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SIEVE_T0 (U V BS)
    (COND ((NULL U) (REVERSIP V))
          (T
           (SIEVE_T0 (CDR U)
            ((LAMBDA (X) (COND ((CDR X) (CONS X V)) (T V)))
             (SIEVE_T2 (CAR U) BS))
            BS)))) 
(PUT 'SIEVE_T1 'NUMBER-OF-ARGS 2) 
(PUT 'SIEVE_T1 'DEFINED-ON-LINE '117) 
(PUT 'SIEVE_T1 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'SIEVE_T1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIEVE_T1 (TT BS)
    (PROG ()
      (SETQ BS *BASIS)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND BS (NULL (TH_MATCH (CAR TT) (CAAR BS))))) (RETURN NIL)))
        (SETQ BS (CDR BS))
        (GO WHILELABEL))
      (COND (BS (RETURN (CONS (CAR TT) (SIEVE_PV (CDR TT) (CDAR BS))))))
      (COND
       ((DL_GET (CADAR TT))
        (PROGN
         (SETQ *BASIS (APPEND (ADDDUMMY (CONS '|:TENSOR| (LIST TT))) *BASIS))
         (SETQ BS *BASIS)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND BS (NULL (TH_MATCH (CAR TT) (CAAR BS))))) (RETURN NIL)))
           (SETQ BS (CDR BS))
           (GO WHILELABEL))
         (COND (BS (RETURN (CONS (CAR TT) (SIEVE_PV (CDR TT) (CDAR BS))))))
         NIL)))
      (RETURN TT))) 
(PUT 'SIEVE_T2 'NUMBER-OF-ARGS 2) 
(PUT 'SIEVE_T2 'DEFINED-ON-LINE '142) 
(PUT 'SIEVE_T2 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'SIEVE_T2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIEVE_T2 (TT BS1)
    (PROG (BS TT1)
      (SETQ BS BS1)
      (COND
       ((DL_GET (CADAR TT)) (SETQ BS (APPEND (ADDDUMMY0 (LIST TT) BS) BS))))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND BS (NULL (TH_MATCH (CAR TT) (CAAR BS))))) (RETURN NIL)))
        (SETQ BS (CDR BS))
        (GO WHILELABEL))
      (SETQ TT1 TT)
      (SETQ PV_DEN 1)
      (COND (BS (SETQ TT (CONS (CAR TT) (SIEVE_PV0 (CDR TT) (CDAR BS) NIL)))))
      (RPLACD (CDAR TT) (TIMES (CDDAR TT) PV_DEN))
      (COND
       (*DEBUG
        (PROGN
         (TERPRI)
         (PROGN (PRIN2 " DEBUG: sieve_t2") NIL)
         (TERPRI)
         (T_PRI1 (CONS '|:TENSOR| (LIST TT1)) T)
         (COND
          (BS
           (PROG (Z)
             (SETQ Z (CDAR BS))
            LAB
             (COND ((NULL Z) (RETURN NIL)))
             ((LAMBDA (Z)
                (T_PRI1 (CONS '|:TENSOR| (LIST (CONS (CAAR BS) Z))) T))
              (CAR Z))
             (SETQ Z (CDR Z))
             (GO LAB))))
         (TERPRI)
         (T_PRI1 (CONS '|:TENSOR| (LIST TT)) T)
         (TERPRI)
         NIL)))
      (RETURN TT))) 
(PUT 'T_ADDF 'NUMBER-OF-ARGS 2) 
(PUT 'T_ADDF 'DEFINED-ON-LINE '167) 
(PUT 'T_ADDF 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_ADDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_ADDF (T1 T2)
    (COND
     ((ORDP (CADR T1) (CADR T2))
      (CONS (CONS (CONS T1 1) 1) (CONS (CONS (CONS T2 1) 1) NIL)))
     (T (T_ADDF T2 T1)))) 
(PUT 'T_ADD2 'NUMBER-OF-ARGS 2) 
(PUT 'T_ADD2 'DEFINED-ON-LINE '173) 
(PUT 'T_ADD2 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_ADD2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_ADD2 (TX1 TX2)
    (PROG (W)
      (SETQ W (IL_UPDATE (CADAR TX2) (DL_GET (CADAR TX1))))
      (SETQ W (PFIND W (CADAR TX1)))
      (SETQ W
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CDR TX2))
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (CONS (CAR X) (PAPPL0 (CDR X) W)))
                                  (CAR X))
                                 NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (X) (CONS (CAR X) (PAPPL0 (CDR X) W)))
                          (CAR X))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (CONS (CAR TX1) (PV_ADD (CDR TX1) W))))) 
(PUT 'T_MATCH 'NUMBER-OF-ARGS 2) 
(PUT 'T_MATCH 'DEFINED-ON-LINE '184) 
(PUT 'T_MATCH 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_MATCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_MATCH (T1 T2) (TH_MATCH (CAR T1) (CAR T2))) 
(PUT 'TH_MATCH 'NUMBER-OF-ARGS 2) 
(PUT 'TH_MATCH 'DEFINED-ON-LINE '186) 
(PUT 'TH_MATCH 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'TH_MATCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TH_MATCH (TH1 TH2)
    (AND (TH_MATCH0 TH1 TH2)
         (EQUAL (LENGTH (DL_GET (CADR TH1))) (LENGTH (DL_GET (CADR TH2)))))) 
(PUT 'TH_MATCH0 'NUMBER-OF-ARGS 2) 
(PUT 'TH_MATCH0 'DEFINED-ON-LINE '190) 
(PUT 'TH_MATCH0 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'TH_MATCH0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TH_MATCH0 (TH1 TH2)
    (AND (EQUAL (CAR TH1) (CAR TH2))
         (EQUAL (LENGTH (CADR TH1)) (LENGTH (CADR TH2))))) 
(PUT 'TH_MATCH_ 'NUMBER-OF-ARGS 2) 
(PUT 'TH_MATCH_ 'DEFINED-ON-LINE '193) 
(PUT 'TH_MATCH_ 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'TH_MATCH_ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TH_MATCH_ (TH1 TH2)
    (COND
     ((AND (EQUAL (CAR TH1) (CAR TH2)) (TH_MATCH1 (CADR TH1) (CADR TH2)))
      (PFIND (CADR TH1) (CADR TH2)))
     (T NIL))) 
(PUT 'TH_MATCH1 'NUMBER-OF-ARGS 2) 
(PUT 'TH_MATCH1 'DEFINED-ON-LINE '198) 
(PUT 'TH_MATCH1 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'TH_MATCH1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TH_MATCH1 (IL1 IL2)
    (COND ((NULL IL1) (NULL IL2))
          ((NULL (EQUAL IL2 (SETQ IL2 (DELETE (CAR IL1) IL2))))
           (TH_MATCH1 (CDR IL1) IL2))
          (T NIL))) 
(PUT 'T_NEG 'NUMBER-OF-ARGS 1) 
(PUT 'T_NEG 'DEFINED-ON-LINE '204) 
(PUT 'T_NEG 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_NEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE T_NEG (TE)
    (COND ((NUMBERP (CAR TE)) (LIST (MINUS (CAR TE))))
          (T
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X TE)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X) (CONS (CAR X) (PV_NEG (CDR X))))
                               (CAR X))
                              NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (X) (CONS (CAR X) (PV_NEG (CDR X)))) (CAR X))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'T_MULT 'NUMBER-OF-ARGS 2) 
(PUT 'T_MULT 'DEFINED-ON-LINE '208) 
(PUT 'T_MULT 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_MULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_MULT (TE1 TE2)
    (COND ((NULL TE1) TE2) ((NUMBERP (CAR TE1)) (C_MULT (CAR TE1) TE2))
          ((NUMBERP (CAR TE2)) (C_MULT (CAR TE2) TE1))
          (T (T_MULT (CDR TE1) (T_MULT1 (CAR TE1) TE2))))) 
(PUT 'T_MULT1 'NUMBER-OF-ARGS 2) 
(PUT 'T_MULT1 'DEFINED-ON-LINE '214) 
(PUT 'T_MULT1 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_MULT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_MULT1 (TE1 TE)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X TE)
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (X) (T_MULT2 TE1 X)) (CAR X)) NIL)))
     LOOPLABEL
      (SETQ X (CDR X))
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (X) (T_MULT2 TE1 X)) (CAR X)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'T_MULT2 'NUMBER-OF-ARGS 2) 
(PUT 'T_MULT2 'DEFINED-ON-LINE '217) 
(PUT 'T_MULT2 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_MULT2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_MULT2 (TT1 TT2)
    (PROG (TT)
      (COND
       ((OR (CDDR TT1) (CDDR TT2))
        (REDERR (LIST 'T_MULT2 " *** Must be tterms: " TT1 TT2))))
      (SETQ TT TT1)
      (SETQ TT1 (T_UPRIGHT TT1 (CAR TT2)))
      (SETQ TT2 (T_UPLEFT TT2 (CAR TT)))
      (RETURN (CONS (CAR TT1) (PV_MULTC (CAADR TT1) (CDR TT2)))))) 
(PUT 'C_MULT 'NUMBER-OF-ARGS 2) 
(PUT 'C_MULT 'DEFINED-ON-LINE '227) 
(PUT 'C_MULT 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'C_MULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE C_MULT (C TE)
    (COND ((NULL TE) NIL) ((NUMBERP (CAR TE)) (LIST (TIMES C (CAR TE))))
          (T
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X TE)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X) (CONS (CAR X) (PV_MULTC C (CDR X))))
                               (CAR X))
                              NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (X) (CONS (CAR X) (PV_MULTC C (CDR X))))
                       (CAR X))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'T_UPRIGHT 'NUMBER-OF-ARGS 2) 
(PUT 'T_UPRIGHT 'DEFINED-ON-LINE '232) 
(PUT 'T_UPRIGHT 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_UPRIGHT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_UPRIGHT (TT TH)
    (PROG (TH1 TT1)
      (SETQ TH1 (CAR TT))
      (SETQ TH1
              (CONS (APPEND (CAR TH1) (CAR TH))
                    (CONS (APPEND (CADR TH1) (CADR TH))
                          (APPEND (CDDR TH1) (CDDR TH)))))
      (RETURN (CONS TH1 (PV_UPRIGHT (CDR TT) (LENGTH (CADR TH))))))) 
(PUT 'T_UPLEFT 'NUMBER-OF-ARGS 2) 
(PUT 'T_UPLEFT 'DEFINED-ON-LINE '240) 
(PUT 'T_UPLEFT 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_UPLEFT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_UPLEFT (TT TH)
    (PROG (TH1 TT1)
      (SETQ TH1 (CAR TT))
      (SETQ TH1
              (CONS (APPEND (CAR TH) (CAR TH1))
                    (CONS (APPEND (CADR TH) (CADR TH1))
                          (APPEND (CDDR TH) (CDDR TH1)))))
      (RETURN (CONS TH1 (PV_UPLEFT (CDR TT) (LENGTH (CADR TH))))))) 
(GLOBAL '(*DEBUG_TIMES)) 
(SWITCH (LIST 'DEBUG_TIMES)) 
(PUT 'B_EXPAND 'NUMBER-OF-ARGS 2) 
(PUT 'B_EXPAND 'DEFINED-ON-LINE '251) 
(PUT 'B_EXPAND 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_EXPAND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE B_EXPAND (U V)
    ((LAMBDA (X) (COND (*DEBUG_TIMES *BASIS) (T (SETQ *BASIS X))))
     (B_EXPAND1 (CADR U) (CADR V) *BASIS *BASIS))) 
(PUT 'B_EXPAND1 'NUMBER-OF-ARGS 4) 
(PUT 'B_EXPAND1 'DEFINED-ON-LINE '255) 
(PUT 'B_EXPAND1 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_EXPAND1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE B_EXPAND1 (T1 T2 BS BS1)
    (COND ((NULL BS) (REVERSIP BS1))
          ((TH_MATCH0 (CAR T1) (CAAR BS))
           (B_EXPAND1 T1 T2 (CDR BS) (B_EXPAND2 (CAR BS) T2 BS1)))
          ((TH_MATCH0 (CAR T2) (CAAR BS))
           (B_EXPAND1 T1 T2 (CDR BS) (B_EXPAND2 (CAR BS) T1 BS1)))
          (T (B_EXPAND1 T1 T2 (CDR BS) BS1)))) 
(PUT 'B_EXPAND2 'NUMBER-OF-ARGS 3) 
(PUT 'B_EXPAND2 'DEFINED-ON-LINE '265) 
(PUT 'B_EXPAND2 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_EXPAND2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE B_EXPAND2 (B T1 BS) (B_EXPAND2B (CAR B) (CDR B) T1 BS)) 
(PUT 'B_EXPAND2B 'NUMBER-OF-ARGS 4) 
(PUT 'B_EXPAND2B 'DEFINED-ON-LINE '272) 
(PUT 'B_EXPAND2B 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_EXPAND2B 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE B_EXPAND2B (TH B T1 BS)
    (COND ((NULL B) BS)
          (T
           (B_EXPAND2B TH (CDR B) T1
            (TSYM2 (LIST (T_PROD (CONS TH (CAR B)) T1)) BS NIL))))) 
(PUT 'B_EXPAND2A 'NUMBER-OF-ARGS 5) 
(PUT 'B_EXPAND2A 'DEFINED-ON-LINE '283) 
(PUT 'B_EXPAND2A 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_EXPAND2A 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE B_EXPAND2A (TH B T1 B1 BS)
    (COND ((NULL B) (B_JOIN (CONS (CAAR B1) (B_EXPAND3 B1 NIL)) BS))
          (T
           (B_EXPAND2A TH (CDR B) T1 (CONS (T_PROD (CONS TH (CAR B)) T1) B1)
            BS)))) 
(PUT 'B_EXPAND3 'NUMBER-OF-ARGS 2) 
(PUT 'B_EXPAND3 'DEFINED-ON-LINE '290) 
(PUT 'B_EXPAND3 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_EXPAND3 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE B_EXPAND3 (B B1)
    (COND ((NULL B) B1) (T (B_EXPAND3 (CDR B) (CONS (CDAR B) B1))))) 
(PUT 'B_JOIN 'NUMBER-OF-ARGS 2) 
(PUT 'B_JOIN 'DEFINED-ON-LINE '294) 
(PUT 'B_JOIN 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_JOIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE B_JOIN (B BS) (B_JOIN1 B BS NIL)) 
(PUT 'B_JOIN1 'NUMBER-OF-ARGS 3) 
(PUT 'B_JOIN1 'DEFINED-ON-LINE '296) 
(PUT 'B_JOIN1 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_JOIN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE B_JOIN1 (B BS BS1)
    (COND ((NULL BS) (REVERSIP (COND (B (CONS B BS1)) (T BS1))))
          ((AND B (TH_MATCH (CAR B) (CAAR BS)))
           (B_JOIN1 NIL (CDR BS)
            (CONS (CONS (CAR B) (B_JOIN2 (CDR B) (CDAR BS))) BS1)))
          (T (B_JOIN1 B (CDR BS) (CONS (CAR BS) BS1))))) 
(PUT 'B_JOIN2 'NUMBER-OF-ARGS 2) 
(PUT 'B_JOIN2 'DEFINED-ON-LINE '302) 
(PUT 'B_JOIN2 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'B_JOIN2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE B_JOIN2 (B1 B2)
    (COND ((NULL B1) B2) (T (B_JOIN2 (CDR B1) (INSERT_PV (CAR B1) B2))))) 
(PUT 'T_PROD 'NUMBER-OF-ARGS 2) 
(PUT 'T_PROD 'DEFINED-ON-LINE '306) 
(PUT 'T_PROD 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'T_PROD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE T_PROD (T1 T2)
    (COND ((NULL (ORDP (CAAR T1) (CAAR T2))) (T_PROD T2 T1))
          (T
           (CONS
            (CONS (APPEND (CAAR T1) (CAAR T2))
                  (CONS (IL_JOIN (CADAR T1) (CADAR T2))
                        (APPEND (CDDAR T1) (CDDAR T2))))
            (CDR (PV_TIMES (CONS '|:PV| (CDR T1)) (CONS '|:PV| (CDR T2)))))))) 
(PUT 'IL_JOIN 'NUMBER-OF-ARGS 2) 
(PUT 'IL_JOIN 'DEFINED-ON-LINE '315) 
(PUT 'IL_JOIN 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'IL_JOIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE IL_JOIN (L1 L2)
    (COND ((NULL L1) L2)
          ((MEMQ (CAR L1) L2) (CONS (WI_NEW (CAR L1)) (IL_JOIN (CDR L1) L2)))
          (T (CONS (CAR L1) (IL_JOIN (CDR L1) L2))))) 
(GLOBAL '(WI_NUMBER)) 
(SETQ WI_NUMBER 0) 
(PUT 'WI_NEW 'NUMBER-OF-ARGS 1) 
(PUT 'WI_NEW 'DEFINED-ON-LINE '323) 
(PUT 'WI_NEW 'DEFINED-IN-FILE 'ATENSOR/TENSOR1.RED) 
(PUT 'WI_NEW 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE WI_NEW (X)
    (PROG (Z)
      (SETQ WI_NUMBER (PLUS WI_NUMBER 1))
      (SETQ Z (INTERN (MKID '|:| WI_NUMBER)))
      (PUT Z 'WINDEX (LIST X))
      (RETURN Z))) 
(ENDMODULE) 