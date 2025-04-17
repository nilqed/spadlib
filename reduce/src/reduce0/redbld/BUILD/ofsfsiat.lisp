(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'OFSFSIAT)) 
(REVISION 'OFSFSIAT
          "$Id: ofsfsiat.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'OFSFSIAT "(c) 1995-2009 A. Dolzmann, T. Sturm, 2017 T. Sturm") 
(PUT 'OFSF_SIMPLAT1 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SIMPLAT1 'DEFINED-ON-LINE '32) 
(PUT 'OFSF_SIMPLAT1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_SIMPLAT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIMPLAT1 (F SOP)
    (PROG (REL LHS)
      (SETQ REL (CAR F))
      (COND
       ((NOT (MEMQ REL '(EQUAL NEQ LEQ GEQ LESSP GREATERP))) (RETURN NIL)))
      (SETQ LHS (CADR F))
      (COND
       ((OR (ATOM LHS) (ATOM (CAR LHS)))
        (RETURN (COND ((OFSF_EVALATP REL LHS) 'TRUE) (T 'FALSE)))))
      (SETQ LHS ((LAMBDA (*EXP) (QUOTF1 LHS (SFTO_DCONTENTF LHS))) T))
      (COND
       ((MINUSF LHS)
        (PROGN (SETQ LHS (NEGF LHS)) (SETQ REL (OFSF_ANEGREL REL)))))
      (COND ((NULL *RLSIATADV) (RETURN (LIST REL LHS NIL))))
      (COND ((EQ REL 'EQUAL) (RETURN (OFSF_SIMPLEQUAL LHS SOP))))
      (COND ((EQ REL 'NEQ) (RETURN (OFSF_SIMPLNEQ LHS SOP))))
      (COND ((EQ REL 'LEQ) (RETURN (OFSF_SIMPLLEQ LHS SOP))))
      (COND ((EQ REL 'GEQ) (RETURN (OFSF_SIMPLGEQ LHS SOP))))
      (COND ((EQ REL 'LESSP) (RETURN (OFSF_SIMPLLESSP LHS SOP))))
      (COND ((EQ REL 'GREATERP) (RETURN (OFSF_SIMPLGREATERP LHS SOP)))))) 
(PUT 'OFSF_SIMPLEQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SIMPLEQUAL 'DEFINED-ON-LINE '59) 
(PUT 'OFSF_SIMPLEQUAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_SIMPLEQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIMPLEQUAL (LHS SOP)
    (PROG (W FF WW)
      (SETQ W (OFSF_POSDEFP LHS))
      (COND ((EQ W 'STSQ) (RETURN 'FALSE)))
      (SETQ FF (SFTO_SQFPARTF LHS))
      (SETQ WW (OFSF_POSDEFP FF))
      (COND ((EQ WW 'STSQ) (RETURN 'FALSE)))
      (COND
       ((AND *RLSITSQSPL (OR *RLSIEXPLA (AND *RLSIEXPL (EQUAL SOP 'AND))))
        (PROGN
         (COND ((EQ WW 'TSQ) (RETURN (OFSF_TSQSPLEQUAL FF))))
         (COND ((EQ W 'TSQ) (RETURN (OFSF_TSQSPLEQUAL LHS)))))))
      (RETURN (OFSF_FACEQUAL* FF SOP)))) 
(PUT 'OFSF_TSQSPLEQUAL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_TSQSPLEQUAL 'DEFINED-ON-LINE '75) 
(PUT 'OFSF_TSQSPLEQUAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_TSQSPLEQUAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_TSQSPLEQUAL (F)
    (PROG (W)
      (SETQ W (OFSF_GETSQSUMMONS F))
      (COND
       ((AND *RLSIFAC (OR *RLSIEXPLA (AND *RLSIEXPL (NULL (CDR W)))))
        (RETURN
         ((LAMBDA (G133)
            (COND ((AND G133 (CDR G133)) (CONS 'AND G133))
                  ((NULL G133) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G133))))
          (PROG (M FORALL-RESULT FORALL-ENDPTR)
            (SETQ M W)
            (COND ((NULL M) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (M)
                                ((LAMBDA (G131)
                                   (COND
                                    ((AND G131 (CDR G131)) (CONS 'OR G131))
                                    ((NULL G131)
                                     (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                                    (T (CAR G131))))
                                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ V M)
                                   (COND ((NULL V) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (V)
                                                       (LIST 'EQUAL V NIL))
                                                     (CAR V))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ V (CDR V))
                                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (V) (LIST 'EQUAL V NIL))
                                             (CAR V))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))))
                              (CAR M))
                             NIL)))
           LOOPLABEL
            (SETQ M (CDR M))
            (COND ((NULL M) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (M)
                        ((LAMBDA (G131)
                           (COND ((AND G131 (CDR G131)) (CONS 'OR G131))
                                 ((NULL G131)
                                  (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                                 (T (CAR G131))))
                         (PROG (V FORALL-RESULT FORALL-ENDPTR)
                           (SETQ V M)
                           (COND ((NULL V) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (V) (LIST 'EQUAL V NIL))
                                             (CAR V))
                                            NIL)))
                          LOOPLABEL
                           (SETQ V (CDR V))
                           (COND ((NULL V) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (V) (LIST 'EQUAL V NIL)) (CAR V))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                      (CAR M))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (RETURN
       ((LAMBDA (G135)
          (COND ((AND G135 (CDR G135)) (CONS 'AND G135))
                ((NULL G135) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR G135))))
        (PROG (M FORALL-RESULT FORALL-ENDPTR)
          (SETQ M W)
          (COND ((NULL M) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (M) (LIST 'EQUAL (SFTO_LMULTF M) NIL))
                            (CAR M))
                           NIL)))
         LOOPLABEL
          (SETQ M (CDR M))
          (COND ((NULL M) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS
                   ((LAMBDA (M) (LIST 'EQUAL (SFTO_LMULTF M) NIL)) (CAR M))
                   NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'OFSF_FACEQUAL* 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_FACEQUAL* 'DEFINED-ON-LINE '86) 
(PUT 'OFSF_FACEQUAL* 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_FACEQUAL* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_FACEQUAL* (F SOP)
    (COND
     ((AND *RLSIFAC (OR *RLSIEXPLA (AND *RLSIEXPL (EQUAL SOP 'OR))))
      (OFSF_FACEQUAL F))
     (T (LIST 'EQUAL F NIL)))) 
(PUT 'OFSF_FACEQUAL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_FACEQUAL 'DEFINED-ON-LINE '92) 
(PUT 'OFSF_FACEQUAL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_FACEQUAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_FACEQUAL (F)
    ((LAMBDA (G137)
       (COND ((AND G137 (CDR G137)) (CONS 'OR G137))
             ((NULL G137) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR G137))))
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X (CDR (SFTO_FCTRF F)))
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (LIST 'EQUAL (CAR X) NIL)) (CAR X))
                             NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (X) (LIST 'EQUAL (CAR X) NIL)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'OFSF_SIMPLNEQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SIMPLNEQ 'DEFINED-ON-LINE '96) 
(PUT 'OFSF_SIMPLNEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_SIMPLNEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIMPLNEQ (LHS SOP)
    (PROG (W FF WW)
      (SETQ W (OFSF_POSDEFP LHS))
      (COND ((EQ W 'STSQ) (RETURN 'TRUE)))
      (SETQ FF (SFTO_SQFPARTF LHS))
      (SETQ WW (OFSF_POSDEFP FF))
      (COND ((EQ WW 'STSQ) (RETURN 'TRUE)))
      (COND
       ((AND *RLSITSQSPL (OR *RLSIEXPLA (AND *RLSIEXPL (EQUAL SOP 'OR))))
        (PROGN
         (COND ((EQ WW 'TSQ) (RETURN (OFSF_TSQSPLNEQ FF))))
         (COND ((EQ W 'TSQ) (RETURN (OFSF_TSQSPLNEQ LHS)))))))
      (RETURN (OFSF_FACNEQ* FF SOP)))) 
(PUT 'OFSF_TSQSPLNEQ 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_TSQSPLNEQ 'DEFINED-ON-LINE '112) 
(PUT 'OFSF_TSQSPLNEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_TSQSPLNEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_TSQSPLNEQ (F)
    (PROG (W)
      (SETQ W (OFSF_GETSQSUMMONS F))
      (COND
       ((AND *RLSIFAC (OR *RLSIEXPLA (AND *RLSIEXPL (NULL (CDR W)))))
        (RETURN
         ((LAMBDA (G141)
            (COND ((AND G141 (CDR G141)) (CONS 'OR G141))
                  ((NULL G141) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G141))))
          (PROG (M FORALL-RESULT FORALL-ENDPTR)
            (SETQ M W)
            (COND ((NULL M) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (M)
                                ((LAMBDA (G139)
                                   (COND
                                    ((AND G139 (CDR G139)) (CONS 'AND G139))
                                    ((NULL G139)
                                     (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                                    (T (CAR G139))))
                                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ V M)
                                   (COND ((NULL V) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (V)
                                                       (LIST 'NEQ V NIL))
                                                     (CAR V))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ V (CDR V))
                                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (V) (LIST 'NEQ V NIL))
                                             (CAR V))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))))
                              (CAR M))
                             NIL)))
           LOOPLABEL
            (SETQ M (CDR M))
            (COND ((NULL M) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (M)
                        ((LAMBDA (G139)
                           (COND ((AND G139 (CDR G139)) (CONS 'AND G139))
                                 ((NULL G139)
                                  (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                                 (T (CAR G139))))
                         (PROG (V FORALL-RESULT FORALL-ENDPTR)
                           (SETQ V M)
                           (COND ((NULL V) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (V) (LIST 'NEQ V NIL))
                                             (CAR V))
                                            NIL)))
                          LOOPLABEL
                           (SETQ V (CDR V))
                           (COND ((NULL V) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (V) (LIST 'NEQ V NIL)) (CAR V))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                      (CAR M))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL))))))
      (RETURN
       ((LAMBDA (G143)
          (COND ((AND G143 (CDR G143)) (CONS 'OR G143))
                ((NULL G143) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                (T (CAR G143))))
        (PROG (M FORALL-RESULT FORALL-ENDPTR)
          (SETQ M W)
          (COND ((NULL M) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  (SETQ FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (M) (LIST 'NEQ (SFTO_LMULTF M) NIL))
                            (CAR M))
                           NIL)))
         LOOPLABEL
          (SETQ M (CDR M))
          (COND ((NULL M) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR
                  (CONS ((LAMBDA (M) (LIST 'NEQ (SFTO_LMULTF M) NIL)) (CAR M))
                        NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL)))))) 
(PUT 'OFSF_FACNEQ* 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_FACNEQ* 'DEFINED-ON-LINE '123) 
(PUT 'OFSF_FACNEQ* 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_FACNEQ* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_FACNEQ* (F SOP)
    (COND
     ((AND *RLSIFAC (OR *RLSIEXPLA (AND *RLSIEXPL (EQUAL SOP 'AND))))
      (OFSF_FACNEQ F))
     (T (LIST 'NEQ F NIL)))) 
(PUT 'OFSF_FACNEQ 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_FACNEQ 'DEFINED-ON-LINE '129) 
(PUT 'OFSF_FACNEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_FACNEQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_FACNEQ (F)
    ((LAMBDA (G145)
       (COND ((AND G145 (CDR G145)) (CONS 'AND G145))
             ((NULL G145) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
             (T (CAR G145))))
     (PROG (X FORALL-RESULT FORALL-ENDPTR)
       (SETQ X (CDR (SFTO_FCTRF F)))
       (COND ((NULL X) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS ((LAMBDA (X) (LIST 'NEQ (CAR X) NIL)) (CAR X))
                             NIL)))
      LOOPLABEL
       (SETQ X (CDR X))
       (COND ((NULL X) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS ((LAMBDA (X) (LIST 'NEQ (CAR X) NIL)) (CAR X)) NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'OFSF_GETSQSUMMONS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_GETSQSUMMONS 'DEFINED-ON-LINE '133) 
(PUT 'OFSF_GETSQSUMMONS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_GETSQSUMMONS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_GETSQSUMMONS (F)
    (PROG (V W)
      (COND ((NULL F) (RETURN NIL)))
      (COND ((OR (ATOM F) (ATOM (CAR F))) (RETURN (LIST NIL))))
      (SETQ W (OFSF_GETSQSUMMONS (CDR F)))
      (SETQ V (LIST (CONS (CONS (CAAAR F) 1) 1)))
      (PROG (X)
        (SETQ X (OFSF_GETSQSUMMONS (CDAR F)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ W (CONS (CONS V X) W))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN W))) 
(PUT 'OFSF_SIMPLLEQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SIMPLLEQ 'DEFINED-ON-LINE '146) 
(PUT 'OFSF_SIMPLLEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_SIMPLLEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIMPLLEQ (LHS SOP)
    (PROG (S1 S2 W X)
      (COND ((EQ (SETQ S1 (OFSF_POSDEFP LHS)) 'STSQ) (RETURN 'FALSE)))
      (SETQ W (SFTO_SQFPARTF LHS))
      (COND ((EQ (SETQ S2 (OFSF_POSDEFP W)) 'STSQ) (RETURN 'FALSE)))
      (COND
       ((AND *RLSITSQSPL (OR *RLSIEXPLA (AND *RLSIEXPL (EQUAL SOP 'AND))))
        (PROGN
         (COND (S2 (RETURN (OFSF_TSQSPLEQUAL W))))
         (COND (S1 (RETURN (OFSF_TSQSPLEQUAL LHS)))))))
      (COND ((OR S1 S2) (RETURN (OFSF_FACEQUAL* W SOP))))
      (COND
       ((AND (NULL *RLSIPD) (NULL *RLSIFACO)) (RETURN (LIST 'LEQ LHS NIL))))
      (SETQ X (SFTO_PDECF LHS))
      (SETQ S1 (OFSF_POSDEFP (CAR X)))
      (COND ((EQ S1 'STSQ) (RETURN (OFSF_FACEQUAL* (CDR X) SOP))))
      (COND (S1 (RETURN (OFSF_FACEQUAL* W SOP))))
      (COND ((EQ (OFSF_POSDEFP (CDR X)) 'STSQ) (SETCDR X 1)))
      (COND
       (*RLSIFACO
        (PROGN
         (SETCAR X (SFTO_LMULTF (OFSF_FACSIMPL (CAR X))))
         (SETCDR X (OFSF_FACSIMPL (CDR X)))))
       (T (SETCDR X (COND ((NOT (EQN (CDR X) 1)) (LIST (CDR X)))))))
      (COND ((EQ (OFSF_POSDEFP (CAR X)) 'STSQ) (SETCAR X 1)))
      (SETQ W (SFTO_LMULTF (CDR X)))
      (COND ((EQ (OFSF_POSDEFP W) 'STSQ) (PROGN (SETCDR X NIL) (SETQ W 1))))
      (COND ((AND (EQN (CAR X) 1) (EQN W 1)) (RETURN 'FALSE)))
      (COND
       ((OR *RLSIEXPLA (AND *RLSIEXPL (EQ SOP 'OR)))
        (RETURN
         ((LAMBDA (G147)
            (COND ((AND G147 (CDR G147)) (CONS 'OR G147))
                  ((NULL G147) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G147))))
          (CONS (LIST 'LEQ (CAR X) NIL)
                (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                  (SETQ FAC (CDR X))
                  (COND ((NULL FAC) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (FAC) (LIST 'EQUAL FAC NIL))
                                    (CAR FAC))
                                   NIL)))
                 LOOPLABEL
                  (SETQ FAC (CDR FAC))
                  (COND ((NULL FAC) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (FAC) (LIST 'EQUAL FAC NIL)) (CAR FAC))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
      (RETURN
       (LIST 'LEQ
             ((LAMBDA (G149)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR X) G149))
                      (T (POLY-MULTF (CAR X) G149))))
              (EXPTF W 2))
             NIL)))) 
(PUT 'OFSF_FACSIMPL 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_FACSIMPL 'DEFINED-ON-LINE '193) 
(PUT 'OFSF_FACSIMPL 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_FACSIMPL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_FACSIMPL (U)
    (PROG (X FORALL-RESULT FORALL-ENDPTR)
      (SETQ X (CDR (SFTO_FCTRF U)))
     STARTOVER
      (COND ((NULL X) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (X)
                 (COND
                  ((NOT (EQ (OFSF_POSDEFP (CAR X)) 'STSQ)) (LIST (CAR X)))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ X (CDR X))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL X) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (X)
                 (COND
                  ((NOT (EQ (OFSF_POSDEFP (CAR X)) 'STSQ)) (LIST (CAR X)))))
               (CAR X)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ X (CDR X))
      (GO LOOPLABEL))) 
(PUT 'OFSF_SIMPLGEQ 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SIMPLGEQ 'DEFINED-ON-LINE '198) 
(PUT 'OFSF_SIMPLGEQ 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_SIMPLGEQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIMPLGEQ (LHS SOP)
    (PROG (X W S1 S2)
      (COND
       ((OR (OFSF_POSDEFP LHS) (OFSF_POSDEFP (SFTO_SQFPARTF LHS)))
        (RETURN 'TRUE)))
      (COND ((AND (NOT *RLSIPD) (NOT *RLSIFACO)) (RETURN (LIST 'GEQ LHS NIL))))
      (SETQ X (SFTO_PDECF LHS))
      (COND ((OFSF_POSDEFP (CAR X)) (RETURN 'TRUE)))
      (COND ((EQ (OFSF_POSDEFP (CDR X)) 'STSQ) (SETCDR X 1)))
      (COND
       (*RLSIFACO
        (PROGN
         (SETCAR X (SFTO_LMULTF (OFSF_FACSIMPL (CAR X))))
         (SETCDR X (OFSF_FACSIMPL (CDR X)))))
       (T (SETCDR X (COND ((NOT (EQN (CDR X) 1)) (LIST (CDR X)))))))
      (SETQ W (SFTO_LMULTF (CDR X)))
      (SETQ S1 (OFSF_POSDEFP (CAR X)))
      (SETQ S2 (OFSF_POSDEFP W))
      (COND ((AND S1 S2) (RETURN 'TRUE)))
      (COND ((EQ S1 'STSQ) (SETCAR X 1))
            ((EQ S2 'STSQ) (PROGN (SETCDR X NIL) (SETQ W 1))))
      (COND
       ((OR *RLSIEXPLA (AND *RLSIEXPL (EQ SOP 'OR)))
        (RETURN
         ((LAMBDA (G151)
            (COND ((AND G151 (CDR G151)) (CONS 'OR G151))
                  ((NULL G151) (COND ((EQ 'OR 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G151))))
          (CONS (LIST 'GEQ (CAR X) NIL)
                (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                  (SETQ FAC (CDR X))
                  (COND ((NULL FAC) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (FAC) (LIST 'EQUAL FAC NIL))
                                    (CAR FAC))
                                   NIL)))
                 LOOPLABEL
                  (SETQ FAC (CDR FAC))
                  (COND ((NULL FAC) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (FAC) (LIST 'EQUAL FAC NIL)) (CAR FAC))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
      (RETURN
       (LIST 'GEQ
             ((LAMBDA (G153)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR X) G153))
                      (T (POLY-MULTF (CAR X) G153))))
              (EXPTF W 2))
             NIL)))) 
(PUT 'OFSF_SIMPLLESSP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SIMPLLESSP 'DEFINED-ON-LINE '235) 
(PUT 'OFSF_SIMPLLESSP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_SIMPLLESSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIMPLLESSP (LHS SOP)
    (PROG (X W S1 S2)
      (COND
       ((OR (OFSF_POSDEFP LHS) (OFSF_POSDEFP (SFTO_SQFPARTF LHS)))
        (RETURN 'FALSE)))
      (COND
       ((AND (NOT *RLSIPD) (NOT *RLSIFACO)) (RETURN (LIST 'LESSP LHS NIL))))
      (SETQ X (SFTO_PDECF LHS))
      (COND ((OFSF_POSDEFP (CAR X)) (RETURN 'FALSE)))
      (COND ((EQ (OFSF_POSDEFP (CDR X)) 'STSQ) (SETCDR X 1)))
      (COND
       (*RLSIFACO
        (PROGN
         (SETCAR X (SFTO_LMULTF (OFSF_FACSIMPL (CAR X))))
         (SETCDR X (OFSF_FACSIMPL (CDR X)))))
       (T (SETCDR X (COND ((NOT (EQN (CDR X) 1)) (LIST (CDR X)))))))
      (SETQ W (SFTO_LMULTF (CDR X)))
      (SETQ S1 (OFSF_POSDEFP (CAR X)))
      (SETQ S2 (OFSF_POSDEFP W))
      (COND ((AND S1 S2) (RETURN 'FALSE)))
      (COND ((EQ S1 'STSQ) (SETCAR X 1))
            ((EQ S2 'STSQ) (PROGN (SETCDR X NIL) (SETQ W 1))))
      (COND
       ((OR *RLSIEXPLA (AND *RLSIEXPL (EQ SOP 'AND)))
        (RETURN
         ((LAMBDA (G155)
            (COND ((AND G155 (CDR G155)) (CONS 'AND G155))
                  ((NULL G155) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G155))))
          (CONS (LIST 'LESSP (CAR X) NIL)
                (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                  (SETQ FAC (CDR X))
                  (COND ((NULL FAC) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (FAC) (LIST 'NEQ FAC NIL))
                                    (CAR FAC))
                                   NIL)))
                 LOOPLABEL
                  (SETQ FAC (CDR FAC))
                  (COND ((NULL FAC) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (FAC) (LIST 'NEQ FAC NIL)) (CAR FAC))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
      (RETURN
       (LIST 'LESSP
             ((LAMBDA (G157)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR X) G157))
                      (T (POLY-MULTF (CAR X) G157))))
              (EXPTF W 2))
             NIL)))) 
(PUT 'OFSF_SIMPLGREATERP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_SIMPLGREATERP 'DEFINED-ON-LINE '272) 
(PUT 'OFSF_SIMPLGREATERP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_SIMPLGREATERP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_SIMPLGREATERP (LHS SOP)
    (PROG (S1 S2 W X)
      (COND ((AND *RLPOS (SFTO_VARF LHS)) (RETURN (LIST 'GREATERP LHS NIL))))
      (COND ((EQ (SETQ S1 (OFSF_POSDEFP LHS)) 'STSQ) (RETURN 'TRUE)))
      (SETQ W (SFTO_SQFPARTF LHS))
      (COND ((EQ (SETQ S2 (OFSF_POSDEFP W)) 'STSQ) (RETURN 'TRUE)))
      (COND
       ((AND *RLSITSQSPL (OR *RLSIEXPLA (AND *RLSIEXPL (EQUAL SOP 'OR))))
        (PROGN
         (COND (S2 (RETURN (OFSF_TSQSPLNEQ W))))
         (COND (S1 (RETURN (OFSF_TSQSPLNEQ LHS)))))))
      (COND ((OR S1 S2) (RETURN (OFSF_FACNEQ* W SOP))))
      (COND
       ((AND (NULL *RLSIPD) (NULL *RLSIFACO))
        (RETURN (LIST 'GREATERP LHS NIL))))
      (SETQ X (SFTO_PDECF LHS))
      (SETQ S1 (OFSF_POSDEFP (CAR X)))
      (COND ((EQ S1 'STSQ) (RETURN (OFSF_FACNEQ* (CDR X) SOP))))
      (COND (S1 (RETURN (OFSF_FACNEQ* W SOP))))
      (COND ((EQ (OFSF_POSDEFP (CDR X)) 'STSQ) (SETCDR X 1)))
      (COND
       (*RLSIFACO
        (PROGN
         (SETCAR X (SFTO_LMULTF (OFSF_FACSIMPL (CAR X))))
         (SETCDR X (OFSF_FACSIMPL (CDR X)))))
       (T (SETCDR X (COND ((NOT (EQN (CDR X) 1)) (LIST (CDR X)))))))
      (COND ((EQ (OFSF_POSDEFP (CAR X)) 'STSQ) (SETCAR X 1)))
      (SETQ W (SFTO_LMULTF (CDR X)))
      (COND ((EQ (OFSF_POSDEFP W) 'STSQ) (PROGN (SETCDR X NIL) (SETQ W 1))))
      (COND ((AND (EQN (CAR X) 1) (EQN W 1)) (RETURN 'TRUE)))
      (COND
       ((OR *RLSIEXPLA (AND *RLSIEXPL (EQ SOP 'AND)))
        (RETURN
         ((LAMBDA (G159)
            (COND ((AND G159 (CDR G159)) (CONS 'AND G159))
                  ((NULL G159) (COND ((EQ 'AND 'AND) 'TRUE) (T 'FALSE)))
                  (T (CAR G159))))
          (CONS (LIST 'GREATERP (CAR X) NIL)
                (PROG (FAC FORALL-RESULT FORALL-ENDPTR)
                  (SETQ FAC (CDR X))
                  (COND ((NULL FAC) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (FAC) (LIST 'NEQ FAC NIL))
                                    (CAR FAC))
                                   NIL)))
                 LOOPLABEL
                  (SETQ FAC (CDR FAC))
                  (COND ((NULL FAC) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (FAC) (LIST 'NEQ FAC NIL)) (CAR FAC))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
      (RETURN
       (LIST 'GREATERP
             ((LAMBDA (G161)
                (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR X) G161))
                      (T (POLY-MULTF (CAR X) G161))))
              (EXPTF W 2))
             NIL)))) 
(PUT 'OFSF_EVALATP 'NUMBER-OF-ARGS 2) 
(PUT 'OFSF_EVALATP 'DEFINED-ON-LINE '321) 
(PUT 'OFSF_EVALATP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_EVALATP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE OFSF_EVALATP (REL LHS)
    (COND ((EQ REL 'EQUAL) (NULL LHS)) ((EQ REL 'NEQ) (NOT (NULL LHS)))
          ((EQ REL 'LEQ) (OR (MINUSF LHS) (NULL LHS)))
          ((EQ REL 'GEQ) (NOT (MINUSF LHS))) ((EQ REL 'LESSP) (MINUSF LHS))
          ((EQ REL 'GREATERP) (NOT (OR (MINUSF LHS) (NULL LHS))))
          (T (REDERR (LIST "ofsf_evalatp: unknown operator " REL))))) 
(PUT 'OFSF_POSDEFP 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_POSDEFP 'DEFINED-ON-LINE '333) 
(PUT 'OFSF_POSDEFP 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_POSDEFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_POSDEFP (U) (COND (*RLPOS (OFSF_POSDEFP-POS U)) (T (SFTO_TSQSUMF U)))) 
(PUT 'OFSF_POSDEFP-POS 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_POSDEFP-POS 'DEFINED-ON-LINE '341) 
(PUT 'OFSF_POSDEFP-POS 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_POSDEFP-POS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_POSDEFP-POS (U) (COND ((NULL U) 'TSQ) (T (OFSF_POSDEFP-POS1 U)))) 
(PUT 'OFSF_POSDEFP-POS1 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_POSDEFP-POS1 'DEFINED-ON-LINE '349) 
(PUT 'OFSF_POSDEFP-POS1 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_POSDEFP-POS1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_POSDEFP-POS1 (U)
    (COND ((OR (ATOM U) (ATOM (CAR U))) (COND ((NOT (MINUSF U)) 'STSQ)))
          (T (AND (OFSF_POSDEFP-POS1 (CDAR U)) (OFSF_POSDEFP-POS1 (CDR U)))))) 
(PUT 'OFSF_SIGNAT 'NUMBER-OF-ARGS 1) 
(PUT 'OFSF_SIGNAT 'DEFINED-ON-LINE '358) 
(PUT 'OFSF_SIGNAT 'DEFINED-IN-FILE 'REDLOG/OFSF/OFSFSIAT.RED) 
(PUT 'OFSF_SIGNAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE OFSF_SIGNAT (AT)
    (LIST (CAR AT)
          (CAR (SIMP (REVAL1 (LIST 'SIGN (MK*SQ (CONS (CADR AT) 1))) T))) NIL)) 
(ENDMODULE) 