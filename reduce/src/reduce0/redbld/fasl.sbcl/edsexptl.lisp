(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSEXPTL)) 
(LOAD_PACKAGE (LIST 'XIDEAL)) 
(FLUID '(*VAROPT *ARBVARS XVARS* *ALLBRANCH)) 
(FLAG '(INDEXNAMES) 'OPFN) 
(PUT 'INDEXNAMES 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXNAMES 'DEFINED-ON-LINE '46) 
(PUT 'INDEXNAMES 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'INDEXNAMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXNAMES (U)
    (PROG ()
      (SETQ U
              (CONS 'LIST
                    (UNIQIDS
                     (PROG (K FORALL-RESULT FORALL-ENDPTR)
                       (SETQ K (GETRLIST U))
                       (COND ((NULL K) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (K) (*A2K K)) (CAR K))
                                             NIL)))
                      LOOPLABEL
                       (SETQ K (CDR K))
                       (COND ((NULL K) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (K) (*A2K K)) (CAR K)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (APPLY1 'INDEXRANGE (LIST (LIST 'EQUAL (GENSYM) U)))
      (RETURN U))) 
(PUT 'SYMBOL_RELATIONS 'NUMBER-OF-ARGS 2) 
(FLAG '(SYMBOL_RELATIONS) 'OPFN) 
(PUT 'SYMBOL_RELATIONS 'DEFINED-ON-LINE '54) 
(PUT 'SYMBOL_RELATIONS 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'SYMBOL_RELATIONS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYMBOL_RELATIONS (S NAME)
    (PROG (TBL IX SYS PIS *VAROPT *ARBVARS)
      (AEVAL (PFORM (LIST (LIST 'EQUAL (LIST NAME 'I 'J) 1))))
      (SETQ TBL (AEVAL (LIST 'TABLEAU S)))
      (SETQ IX (AEVAL (LIST 'INDEXNAMES (LIST 'INDEPENDENCE S))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST (LIST 'LENGTH TBL))) I))
          (RETURN NIL)))
        (AEVAL* (INDEXRANGE (LIST (LIST 'EQUAL '!SYMBOL!INDEX I))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETQ PIS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST (LIST 'LENGTH TBL)))
                         I))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ J (GETRLIST (AEVAL* IX)))
                                   (COND ((NULL J) (RETURN (MAKELIST NIL))))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (J)
                                                       (AEVAL*
                                                        (LIST NAME I
                                                              (LIST 'MINUS
                                                                    J))))
                                                     (CAR J))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ J (CDR J))
                                   (COND
                                    ((NULL J)
                                     (RETURN (CONS 'LIST FORALL-RESULT))))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (J)
                                               (AEVAL*
                                                (LIST NAME I (LIST 'MINUS J))))
                                             (CAR J))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 NIL)))
               LOOPLABEL
                (SETQ I
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         I))
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST (LIST 'LENGTH TBL)))
                         I))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         (PROG (J FORALL-RESULT FORALL-ENDPTR)
                           (SETQ J (GETRLIST (AEVAL* IX)))
                           (COND ((NULL J) (RETURN (MAKELIST NIL))))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (J)
                                               (AEVAL*
                                                (LIST NAME I (LIST 'MINUS J))))
                                             (CAR J))
                                            NIL)))
                          LOOPLABEL
                           (SETQ J (CDR J))
                           (COND
                            ((NULL J) (RETURN (CONS 'LIST FORALL-RESULT))))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (J)
                                       (AEVAL* (LIST NAME I (LIST 'MINUS J))))
                                     (CAR J))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SYS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
               STARTOVER
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST (LIST 'LENGTH TBL)))
                         I))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (PROG (J FORALL-RESULT FORALL-ENDPTR)
                          (SETQ J 1)
                          (COND
                           ((|AMINUSP:|
                             (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH IX)) J))
                            (RETURN (MAKELIST NIL))))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           (AEVAL*
                                            (LIST 'DIFFERENCE (LIST TBL I J)
                                                  (LIST 'PART PIS I J)))
                                           NIL)))
                         LOOPLABEL
                          (SETQ J
                                  ((LAMBDA (FORALL-RESULT)
                                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                   J))
                          (COND
                           ((|AMINUSP:|
                             (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH IX)) J))
                            (RETURN (CONS 'LIST FORALL-RESULT))))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   (AEVAL*
                                    (LIST 'DIFFERENCE (LIST TBL I J)
                                          (LIST 'PART PIS I J)))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                (SETQ I
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         I))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND
                 ((|AMINUSP:|
                   (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST (LIST 'LENGTH TBL)))
                         I))
                  (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (GETRLIST
                         (PROG (J FORALL-RESULT FORALL-ENDPTR)
                           (SETQ J 1)
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH IX)) J))
                             (RETURN (MAKELIST NIL))))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            (AEVAL*
                                             (LIST 'DIFFERENCE (LIST TBL I J)
                                                   (LIST 'PART PIS I J)))
                                            NIL)))
                          LOOPLABEL
                           (SETQ J
                                   ((LAMBDA (FORALL-RESULT)
                                      (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                    J))
                           (COND
                            ((|AMINUSP:|
                              (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH IX)) J))
                             (RETURN (CONS 'LIST FORALL-RESULT))))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    (AEVAL*
                                     (LIST 'DIFFERENCE (LIST TBL I J)
                                           (LIST 'PART PIS I J)))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         I))
                (GO LOOPLABEL)))
      (SETQ PIS
              (PROG (L FORALL-RESULT FORALL-ENDPTR)
                (SETQ L (GETRLIST (AEVAL PIS)))
               STARTOVER
                (COND ((NULL L) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT ((LAMBDA (L) (AEVAL L)) (CAR L)))
                (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                (SETQ L (CDR L))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL L) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (GETRLIST ((LAMBDA (L) (AEVAL L)) (CAR L))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ L (CDR L))
                (GO LOOPLABEL)))
      (SETQ SYS
              (AEVAL
               (LIST 'FIRST
                     (LIST 'SOLVE SYS (LIST 'APPEND (LIST 'COBASIS S) PIS)))))
      (SETQ SYS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (GETRLIST (AEVAL SYS)))
               STARTOVER
                (COND ((NULL X) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X)
                           (COND
                            ((MEMBER (REVALX (LIST 'LHS X)) (REVALX PIS))
                             (AEVAL
                              (LIST 'LIST
                                    (LIST 'DIFFERENCE (LIST 'LHS X)
                                          (LIST 'RHS X)))))
                            (T (AEVAL (LIST 'LIST)))))
                         (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (GETRLIST
                         ((LAMBDA (X)
                            (COND
                             ((MEMBER (REVALX (LIST 'LHS X)) (REVALX PIS))
                              (AEVAL
                               (LIST 'LIST
                                     (LIST 'DIFFERENCE (LIST 'LHS X)
                                           (LIST 'RHS X)))))
                             (T (AEVAL (LIST 'LIST)))))
                          (CAR X))))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL)))
      (RETURN (AEVAL SYS)))) 
(PUT 'SYMBOL_MATRIX 'NUMBER-OF-ARGS 2) 
(FLAG '(SYMBOL_MATRIX) 'OPFN) 
(PUT 'SYMBOL_MATRIX 'DEFINED-ON-LINE '74) 
(PUT 'SYMBOL_MATRIX 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'SYMBOL_MATRIX 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SYMBOL_MATRIX (S NAME)
    (PROG (SYS WLIST N)
      (AEVAL
       (PFORM
        (LIST (LIST 'EQUAL (LIST NAME 'I) 0)
              (LIST 'EQUAL
                    (LIST 'LIST (LIST '!SYMBOL!PI 'I 'J) (LIST '!SYMBOL!W 'I))
                    1))))
      (SETQ N (AEVAL (LIST 'FIRST (LIST 'LENGTH (LIST 'TABLEAU S)))))
      (SETQ WLIST
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (AEVAL* (LIST '!SYMBOL!W I)) NIL)))
               LOOPLABEL
                (SETQ I
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         I))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST '!SYMBOL!W I)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SYS (AEVAL (LIST 'SYMBOL_RELATIONS S '!SYMBOL!PI)))
      (SETK 'RL
            (PROG (I FORALL-RESULT FORALL-ENDPTR)
              (SETQ I 1)
             STARTOVER
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I))
                (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                        (SETQ J
                                (GETRLIST
                                 (AEVAL*
                                  (LIST 'INDEXNAMES (LIST 'INDEPENDENCE S)))))
                        (COND ((NULL J) (RETURN (MAKELIST NIL))))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (J)
                                            (AEVAL*
                                             (LIST 'MAKE_RULE
                                                   (LIST '!SYMBOL!PI I
                                                         (LIST 'MINUS J))
                                                   (LIST 'TIMES
                                                         (LIST '!SYMBOL!W I)
                                                         (LIST NAME
                                                               (LIST 'MINUS
                                                                     J))))))
                                          (CAR J))
                                         NIL)))
                       LOOPLABEL
                        (SETQ J (CDR J))
                        (COND ((NULL J) (RETURN (CONS 'LIST FORALL-RESULT))))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (AEVAL*
                                     (LIST 'MAKE_RULE
                                           (LIST '!SYMBOL!PI I (LIST 'MINUS J))
                                           (LIST 'TIMES (LIST '!SYMBOL!W I)
                                                 (LIST NAME
                                                       (LIST 'MINUS J))))))
                                  (CAR J))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL)))
              (SETQ FORALL-ENDPTR (LASTPAIR (CONS 'LIST FORALL-RESULT)))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
             LOOPLABEL
              (COND
               ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* N) I))
                (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (GETRLIST
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J
                                 (GETRLIST
                                  (AEVAL*
                                   (LIST 'INDEXNAMES (LIST 'INDEPENDENCE S)))))
                         (COND ((NULL J) (RETURN (MAKELIST NIL))))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J)
                                             (AEVAL*
                                              (LIST 'MAKE_RULE
                                                    (LIST '!SYMBOL!PI I
                                                          (LIST 'MINUS J))
                                                    (LIST 'TIMES
                                                          (LIST '!SYMBOL!W I)
                                                          (LIST NAME
                                                                (LIST 'MINUS
                                                                      J))))))
                                           (CAR J))
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (CDR J))
                         (COND ((NULL J) (RETURN (CONS 'LIST FORALL-RESULT))))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (J)
                                     (AEVAL*
                                      (LIST 'MAKE_RULE
                                            (LIST '!SYMBOL!PI I
                                                  (LIST 'MINUS J))
                                            (LIST 'TIMES (LIST '!SYMBOL!W I)
                                                  (LIST NAME
                                                        (LIST 'MINUS J))))))
                                   (CAR J))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (GO LOOPLABEL)))
      (AEVAL (LET '(RL)))
      (SETQ SYS (AEVAL SYS))
      (AEVAL (CLEARRULES (LIST 'RL)))
      (AEVAL
       (MATRIX
        (LIST (LIST '!SYMBOL!MAT (LIST 'LENGTH SYS) (LIST 'LENGTH WLIST)))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH SYS)) I))
          (RETURN NIL)))
        (PROG (J)
          (SETQ J 1)
         LAB
          (COND
           ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH WLIST)) J))
            (RETURN NIL)))
          (SETK (LIST '!SYMBOL!MAT I J)
                (AEVAL*
                 (LIST 'COEFFN (LIST 'PART SYS I) (LIST 'PART WLIST J) 1)))
          (SETQ J
                  ((LAMBDA (FORALL-RESULT)
                     (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                   J))
          (GO LAB))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (RETURN (AEVAL '!SYMBOL!MAT)))) 
(PUT 'CHARACTERISTIC_VARIETY 'NUMBER-OF-ARGS 2) 
(FLAG '(CHARACTERISTIC_VARIETY) 'OPFN) 
(PUT 'CHARACTERISTIC_VARIETY 'DEFINED-ON-LINE '97) 
(PUT 'CHARACTERISTIC_VARIETY 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'CHARACTERISTIC_VARIETY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHARACTERISTIC_VARIETY (S NAME)
    (PROG (IX M SYS XVARS*)
      (SETQ IX (AEVAL (LIST 'INDEXNAMES (LIST 'INDEPENDENCE S))))
      (SETQ M (AEVAL (LIST 'SYMBOL_MATRIX S NAME)))
      (COND
       ((EVALGREATERP (AEVAL (LIST 'FIRST (LIST 'LENGTH M)))
                      (AEVAL (LIST 'SECOND (LIST 'LENGTH M))))
        (SETQ M (AEVAL (LIST 'TP M)))))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'DIFFERENCE (AEVAL* (LIST 'SECOND (LIST 'LENGTH M))) I))
          (RETURN NIL)))
        (AEVAL* (INDEXRANGE (LIST (LIST 'EQUAL 'SYMBOL!INDEX! I))))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (SETK 'WLIST
            (PROG (I FORALL-RESULT FORALL-ENDPTR)
              (SETQ I 1)
              (COND
               ((|AMINUSP:|
                 (LIST 'DIFFERENCE (AEVAL* (LIST 'SECOND (LIST 'LENGTH M))) I))
                (RETURN (MAKELIST NIL))))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS (AEVAL* (LIST '!SYMBOL!W I)) NIL)))
             LOOPLABEL
              (SETQ I
                      ((LAMBDA (FORALL-RESULT)
                         (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                       I))
              (COND
               ((|AMINUSP:|
                 (LIST 'DIFFERENCE (AEVAL* (LIST 'SECOND (LIST 'LENGTH M))) I))
                (RETURN (CONS 'LIST FORALL-RESULT))))
              (RPLACD FORALL-ENDPTR (CONS (AEVAL* (LIST '!SYMBOL!W I)) NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL)))
      (SETK 'WWW (AEVAL 1))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND
         ((|AMINUSP:|
           (LIST 'DIFFERENCE (AEVAL* (LIST 'FIRST (LIST 'LENGTH M))) I))
          (RETURN NIL)))
        (SETK 'WWW
              (AEVAL*
               (LIST 'WEDGE
                     (PROG (J FORALL-RESULT)
                       (SETQ J 1)
                       (SETQ FORALL-RESULT 0)
                      LAB1
                       (COND
                        ((|AMINUSP:|
                          (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH 'WLIST)) J))
                         (RETURN FORALL-RESULT)))
                       (SETQ FORALL-RESULT
                               (AEVAL*
                                (LIST 'PLUS
                                      (AEVAL*
                                       (LIST 'TIMES (LIST M I J)
                                             (LIST '!SYMBOL!W J)))
                                      FORALL-RESULT)))
                       (SETQ J
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                J))
                       (GO LAB1))
                     'WWW)))
        (SETQ I
                ((LAMBDA (FORALL-RESULT) (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                 I))
        (GO LAB))
      (RETURN
       (AEVAL
        (LIST 'LIST (LIST 'EXCOEFFS 'WWW)
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I (GETRLIST (AEVAL IX)))
                (COND ((NULL I) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (I)
                                    (AEVAL (LIST NAME (LIST 'MINUS I))))
                                  (CAR I))
                                 NIL)))
               LOOPLABEL
                (SETQ I (CDR I))
                (COND ((NULL I) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (I) (AEVAL (LIST NAME (LIST 'MINUS I))))
                          (CAR I))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'MAKE_RULE 'NUMBER-OF-ARGS 2) 
(FLAG '(MAKE_RULE) 'OPFN) 
(PUT 'MAKE_RULE 'DEFINED-ON-LINE '115) 
(PUT 'MAKE_RULE 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'MAKE_RULE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAKE_RULE (LH RH) (LIST 'REPLACEBY LH RH)) 
(FLUID '(*EDSDEBUG PRINT_ FNAME_ TIME_ XVARS* *ALLBRANCH *ARBVARS)) 
(MKFORM* '|EDS:T| 0) 
(PUT 'EDSORDERP 'NUMBER-OF-ARGS 2) 
(FLAG '(EDSORDERP) 'OPFN) 
(PUT 'EDSORDERP 'DEFINED-ON-LINE '126) 
(PUT 'EDSORDERP 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'EDSORDERP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDSORDERP (X Y) (COND ((ORDP (REVALX X) (REVALX Y)) 1) (T 0))) 
(PUT 'INVARIANTS 'PSOPFN 'INVARIANTS) 
(PUT 'INVARIANTS 'NUMBER-OF-ARGS 1) 
(PUT 'INVARIANTS 'DEFINED-ON-LINE '132) 
(PUT 'INVARIANTS 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'INVARIANTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVARIANTS (U)
    (COND
     ((EQUAL (LENGTH U) 2)
      ((LAMBDA (X Y) (AEVAL (LIST 'INVARIANTS0 X Y))) (CAR U) (CADR U)))
     ((EQUAL (LENGTH U) 1)
      ((LAMBDA (X Y) (AEVAL (LIST 'INVARIANTS0 X Y))) (CAR U)
       (CONS 'LIST NIL)))
     (T (REDERR "Wrong number of arguments to invariants")))) 
(PUT 'INVARIANTS0 'NUMBER-OF-ARGS 2) 
(FLAG '(INVARIANTS0) 'OPFN) 
(PUT 'INVARIANTS0 'DEFINED-ON-LINE '141) 
(PUT 'INVARIANTS0 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'INVARIANTS0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INVARIANTS0 (S C)
    (PROG (ANS INV CFRM Z XVARS*)
      (AEVAL (LOAD_PACKAGE (LIST 'ODESOLVE 'CRACK)))
      (AEVAL (LIST 'SETCRACKFLAGS))
      (SETQ CFRM (AEVAL (LIST 'COFRAMING)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'PART S 0)) (AEVAL 'EDS))
        (PROGN
         (AEVAL (LIST 'SET_COFRAMING S))
         (COND
          ((EVALEQUAL (AEVAL C) (AEVAL (LIST 'LIST)))
           (SETQ C (AEVAL (LIST 'COORDINATES S)))))
         (SETQ S (AEVAL (LIST 'SYSTEMEDS S)))))
       (T (SETQ S (AEVAL (LIST 'XAUTO S)))))
      (COND
       ((EVALEQUAL (AEVAL C) (AEVAL (LIST 'LIST)))
        (SETQ C
                (AEVAL
                 (LIST 'REVERSE
                       (LIST 'SORT (LIST 'COORDINATES S) 'EDSORDERP))))))
      (SETQ Z
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A 1)
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH S)) A))
                  (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS (AEVAL* (MKFORM* (MKID '|EDS:U| A) 0))
                                      NIL)))
               LOOPLABEL
                (SETQ A
                        ((LAMBDA (FORALL-RESULT)
                           (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                         A))
                (COND
                 ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* (LIST 'LENGTH S)) A))
                  (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS (AEVAL* (MKFORM* (MKID '|EDS:U| A) 0)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ ANS (AEVAL (LIST 'FOLIATION S C Z)))
      (SETQ INV (AEVAL (LIST 'SOLVE ANS Z)))
      (COND
       ((EVALEQUAL (AEVAL (LIST 'LENGTH Z)) 1)
        (SETQ INV
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (GETRLIST (AEVAL INV)))
                  (COND ((NULL X) (RETURN (MAKELIST NIL))))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (X) (AEVAL (LIST 'LIST X)))
                                    (CAR X))
                                   NIL)))
                 LOOPLABEL
                  (SETQ X (CDR X))
                  (COND ((NULL X) (RETURN (CONS 'LIST FORALL-RESULT))))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (X) (AEVAL (LIST 'LIST X))) (CAR X))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND
       ((BOOLVALUE* (REVALX *EDSDEBUG))
        (ASSGNPRI (AEVAL "Constants") NIL 'ONLY)))
      (COND ((BOOLVALUE* (REVALX *EDSDEBUG)) (ASSGNPRI (AEVAL INV) NIL 'ONLY)))
      (COND
       ((EVALNEQ (AEVAL (LIST 'LENGTH INV)) 1)
        (AEVAL (REDERR (REVALX "Not a unique solution")))))
      (AEVAL (LIST 'SET_COFRAMING CFRM))
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (GETRLIST (AEVAL (LIST 'FIRST INV))))
         (COND ((NULL X) (RETURN (MAKELIST NIL))))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (AEVAL (LIST 'RHS X))) (CAR X))
                               NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN (CONS 'LIST FORALL-RESULT))))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (X) (AEVAL (LIST 'RHS X))) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'FOLIATION 'NUMBER-OF-ARGS 3) 
(FLAG '(FOLIATION) 'OPFN) 
(PUT 'FOLIATION 'DEFINED-ON-LINE '171) 
(PUT 'FOLIATION 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'FOLIATION 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FOLIATION (S C Z)
    (PROG (R N X S0 Z0 G Q F F0 PRINT_ FNAME_ TIME_ *ALLBRANCH *ARBVARS XVARS*)
      (SETQ R (AEVAL (LIST 'LENGTH S)))
      (SETQ N (AEVAL (LIST 'LENGTH C)))
      (SETQ FNAME_ '|EDS:C|)
      (COND
       ((EVALGREATERP (AEVAL R) (AEVAL N))
        (AEVAL
         (RERROR 'EDS 0 (REVALX "Not enough coordinates in foliation")))))
      (COND
       ((EVALNEQ (AEVAL R) (AEVAL (LIST 'LENGTH Z)))
        (AEVAL
         (RERROR 'EDS 0
                 (REVALX "Wrong number of invariant labels in foliation")))))
      (COND
       ((EVALEQUAL (AEVAL R) (AEVAL N))
        (PROGN
         (SETQ G
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A 1)
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* R) A))
                     (RETURN (MAKELIST NIL))))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (AEVAL*
                                     (LIST 'EQUAL (LIST 'PART C A)
                                           (LIST 'PART Z A)))
                                    NIL)))
                  LOOPLABEL
                   (SETQ A
                           ((LAMBDA (FORALL-RESULT)
                              (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                            A))
                   (COND
                    ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* R) A))
                     (RETURN (CONS 'LIST FORALL-RESULT))))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (AEVAL*
                             (LIST 'EQUAL (LIST 'PART C A) (LIST 'PART Z A)))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (AEVAL (EDSDEBUG "Intermediate result" G 'PREFIX))
         (RETURN (AEVAL G)))))
      (SETQ S0 (AEVAL (LIST 'LIST)))
      (SETQ Z0 (AEVAL (LIST 'LIST)))
      (WHILE (EVALLESSP (AEVAL* (LIST 'LENGTH S0)) (AEVAL* R))
             (PROGN
              (SETQ X (AEVAL* (LIST 'FIRST C)))
              (SETQ C (AEVAL* (LIST 'REST C)))
              (SETQ Z0 (AEVAL* (LIST 'CONS X Z0)))
              (SETQ S0
                      (AEVAL*
                       (LIST 'XAUTO
                             (LIST 'XMOD S (LIST 'LIST (LIST 'D X))))))))
      (SETQ C (AEVAL (LIST 'APPEND C (LIST 'REST Z0))))
      (EDSDEBUG "Truncating coordinate : " X 'PREFIX)
      (SETQ G (AEVAL (LIST 'FOLIATION S0 C Z)))
      (PROG (Y)
        (SETQ Y (GETRLIST (AEVAL Z)))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (SETQ Y (*A2K Y))
            (AEVAL (FDOMAIN (LIST (LIST 'EQUAL Y (LIST Y '|EDS:T|)))))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (SETQ S (AEVAL (LIST 'PULLBACK S G)))
      (SETQ S (AEVAL (LIST 'PULLBACK S (LIST 'LIST (LIST 'EQUAL X '|EDS:T|)))))
      (SETQ Q
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (GETRLIST (AEVAL S)))
                (COND ((NULL F) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (AEVAL
                                     (LIST 'INNERPROD (LIST 'PARTDF '|EDS:T|)
                                           F)))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (AEVAL
                             (LIST 'INNERPROD (LIST 'PARTDF '|EDS:T|) F)))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Q
              (AEVAL
               (LIST 'SOLVE Q
                     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                       (SETQ Y (GETRLIST (AEVAL Z)))
                       (COND ((NULL Y) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (Y)
                                           (AEVAL (LIST 'PARTDF Y '|EDS:T|)))
                                         (CAR Y))
                                        NIL)))
                      LOOPLABEL
                       (SETQ Y (CDR Y))
                       (COND ((NULL Y) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (Y) (AEVAL (LIST 'PARTDF Y '|EDS:T|)))
                                 (CAR Y))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (COND ((EVALNEQ (AEVAL R) 1) (SETQ Q (AEVAL (LIST 'FIRST Q)))))
      (SETQ Q
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (GETRLIST (AEVAL Q)))
                (COND ((NULL F) (RETURN (MAKELIST NIL))))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (AEVAL
                                     (LIST 'DIFFERENCE (LIST 'LHS F)
                                           (LIST 'RHS F))))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN (CONS 'LIST FORALL-RESULT))))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (AEVAL
                             (LIST 'DIFFERENCE (LIST 'LHS F) (LIST 'RHS F))))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ Q (AEVAL (LIST 'SUB (LIST 'EQUAL 'PARTDF 'DF) Q)))
      (EDSDEBUG "CRACK ODE" Q 'PREFIX)
      (SETQ Q (AEVAL (LIST 'CRACK Q (LIST 'LIST) Z (LIST 'LIST))))
      (PROG (Y)
        (SETQ Y (GETRLIST (AEVAL Z)))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (PROGN (SETQ Y (*A2K Y)) (AEVAL (MKFORM* Y 0)))) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (EDSDEBUG "CRACK solution" Q 'PREFIX)
      (SETQ F (AEVAL (LIST 'LIST)))
      (WHILE (EVALNEQ (AEVAL* Q) (AEVAL* (LIST 'LIST)))
             (PROGN
              (SETQ F (AEVAL* (LIST 'FIRST Q)))
              (SETQ Q (AEVAL* (LIST 'REST Q)))
              (SETQ Z0 (AEVAL* (LIST 'THIRD F)))
              (COND
               ((AND (EVALEQUAL (AEVAL* (LIST 'FIRST F)) (AEVAL* (LIST 'LIST)))
                     (EVALEQUAL (AEVAL* (LIST 'LENGTH Z0)) (AEVAL* R)))
                (SETQ Q (AEVAL* (LIST 'LIST))))
               ((EVALGREATERP (AEVAL* (LIST 'LENGTH Z0)) (AEVAL* R))
                (COND
                 ((EVALEQUAL
                   (AEVAL*
                    (LIST 'LENGTH
                          (SETQ F0 (AEVAL* (LIST 'SOLVE (LIST 'FIRST F) Z)))))
                   0)
                  (SETQ F (AEVAL* (LIST 'LIST))))
                 (T
                  (PROGN
                   (COND
                    ((EVALEQUAL (AEVAL* R) 1)
                     (SETQ F0
                             (AEVAL*
                              (LIST 'LIST (LIST 'LIST (LIST 'FIRST F0)))))))
                   (SETQ Z0
                           (PROG (V FORALL-RESULT FORALL-ENDPTR)
                             (SETQ V (GETRLIST (AEVAL* Z0)))
                            STARTOVER
                             (COND ((NULL V) (RETURN (MAKELIST NIL))))
                             (SETQ FORALL-RESULT
                                     ((LAMBDA (V)
                                        (COND
                                         ((MEMBER (REVALX V) (REVALX Z))
                                          (AEVAL* (LIST 'LIST)))
                                         (T (AEVAL* (LIST 'LIST V)))))
                                      (CAR V)))
                             (SETQ FORALL-ENDPTR
                                     (LASTPAIR (CONS 'LIST FORALL-RESULT)))
                             (SETQ V (CDR V))
                             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                            LOOPLABEL
                             (COND ((NULL V) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (GETRLIST
                                      ((LAMBDA (V)
                                         (COND
                                          ((MEMBER (REVALX V) (REVALX Z))
                                           (AEVAL* (LIST 'LIST)))
                                          (T (AEVAL* (LIST 'LIST V)))))
                                       (CAR V))))
                             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                             (SETQ V (CDR V))
                             (GO LOOPLABEL)))
                   (SETQ F
                           (AEVAL*
                            (LIST 'LIST (LIST 'LIST)
                                  (LIST 'APPEND (LIST 'SECOND F)
                                        (LIST 'FIRST F0))
                                  Z0)))
                   (SETQ Q (AEVAL* (LIST 'LIST)))))))
               (T (SETQ F (AEVAL* (LIST 'LIST)))))))
      (PROG (Y)
        (SETQ Y (GETRLIST (AEVAL Z)))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (PROGN (SETQ Y (*A2K Y)) (AEVAL (REMFDOMAIN (LIST Y)))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (COND
       ((EVALEQUAL (AEVAL F) (AEVAL (LIST 'LIST)))
        (AEVAL
         (RERROR 'EDS 0
                 (REVALX "Intermediate ODE general solution not found")))))
      (SETQ G (AEVAL (LIST 'SUB (LIST 'SECOND F) G)))
      (SETQ F
              (AEVAL
               (LIST 'CONS (LIST 'EQUAL '|EDS:T| X)
                     (PROG (A FORALL-RESULT FORALL-ENDPTR)
                       (SETQ A 1)
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* R) A))
                         (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        (AEVAL*
                                         (LIST 'EQUAL (LIST 'PART Z0 A)
                                               (LIST 'PART Z A)))
                                        NIL)))
                      LOOPLABEL
                       (SETQ A
                               ((LAMBDA (FORALL-RESULT)
                                  (AEVAL* (LIST 'PLUS FORALL-RESULT 1)))
                                A))
                       (COND
                        ((|AMINUSP:| (LIST 'DIFFERENCE (AEVAL* R) A))
                         (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                (AEVAL*
                                 (LIST 'EQUAL (LIST 'PART Z0 A)
                                       (LIST 'PART Z A)))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ G (AEVAL (LIST 'SUB F G)))
      (EDSDEBUG "Intermediate result" G 'PREFIX)
      (RETURN (AEVAL G)))) 
(PUT 'POINCARE 'NUMBER-OF-ARGS 1) 
(FLAG '(POINCARE) 'OPFN) 
(PUT 'POINCARE 'DEFINED-ON-LINE '245) 
(PUT 'POINCARE 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'POINCARE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POINCARE (DF)
    (PROG (F)
      (AEVAL (PFORM (LIST (LIST 'EQUAL '!LAMBDA! 0))))
      (SETQ F
              (AEVAL
               (LIST 'SUB
                     (PROG (C FORALL-RESULT FORALL-ENDPTR)
                       (SETQ C (GETRLIST (AEVAL (LIST 'COORDINATES DF))))
                       (COND ((NULL C) (RETURN (MAKELIST NIL))))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (C)
                                           (AEVAL
                                            (LIST 'EQUAL C
                                                  (LIST 'TIMES C '!LAMBDA!))))
                                         (CAR C))
                                        NIL)))
                      LOOPLABEL
                       (SETQ C (CDR C))
                       (COND ((NULL C) (RETURN (CONS 'LIST FORALL-RESULT))))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (C)
                                   (AEVAL
                                    (LIST 'EQUAL C (LIST 'TIMES C '!LAMBDA!))))
                                 (CAR C))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     DF)))
      (SETQ F (AEVAL (LIST 'INNERPROD (LIST 'PARTDF '!LAMBDA!) F)))
      (SETQ F (AEVAL (LIST 'INT F '!LAMBDA!)))
      (SETQ F
              (AEVAL
               (LIST 'DIFFERENCE (LIST 'SUB (LIST 'EQUAL '!LAMBDA! 1) F)
                     (LIST 'SUB (LIST 'EQUAL '!LAMBDA! 0) F))))
      (RETURN (AEVAL (LIST 'REVAL F))))) 
(PUT 'INTEGRABILITY 'RTYPEFN 'QUOTELIST) 
(PUT 'INTEGRABILITY 'LISTFN 'EVALINTEGRABILITY) 
(PUT 'EVALINTEGRABILITY 'NUMBER-OF-ARGS 2) 
(PUT 'EVALINTEGRABILITY 'DEFINED-ON-LINE '265) 
(PUT 'EVALINTEGRABILITY 'DEFINED-IN-FILE 'EDS/EDSEXPTL.RED) 
(PUT 'EVALINTEGRABILITY 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALINTEGRABILITY (S V)
    (COND
     ((EDSP (SETQ S (REVAL1 (CAR S) T)))
      (*SYS2A1 (NONPFAFFPART (CADR (EDSPROTECT (LIST 'CLOSURE S)))) V))
     (T
      (AEVAL
       (LIST 'APPEND (LIST 'XMOD S (LIST 'ONE_FORMS S))
             (LIST 'XMOD (LIST 'D S) (LIST 'ONE_FORMS S))))))) 
(ENDMODULE) 