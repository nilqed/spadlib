(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NCFACTOR)) 
(SHARE (LIST 'NC_FACTOR_TIME)) 
(SETQ NC_FACTOR_TIME (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 0)) 
(AEVAL (OPERATOR (LIST 'CC*))) 
(PUT 'NC_FACTORIZE 'NUMBER-OF-ARGS 1) 
(PUT 'NC_FACTORIZE 'DEFINED-ON-LINE '39) 
(PUT 'NC_FACTORIZE 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NC_FACTORIZE (U)
    (PROG (R O *GSUGAR COMM CR CL)
      (SETQ O (APPLY1 'TORDER '(GRADLEX)))
      (NC-GSETUP)
      (SETQ COMM (NC_COMMFACTORS* U))
      (SETQ CL (CAR COMM))
      (SETQ U (CADR COMM))
      (SETQ CR (CADDR COMM))
      (COND ((CONSTANT_EXPRP U) (COND ((NEQ U 1) (SETQ CL (CONS U CL)))))
            (T
             (SETQ R
                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                       (SETQ P (NC_FACTORIZE0 (A2NCVDP U) NIL NIL NIL NIL NIL))
                       (COND ((NULL P) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (P)
                                           (NUM (DIP2A (CADR (CDDR P)))))
                                         (CAR P))
                                        NIL)))
                      LOOPLABEL
                       (SETQ P (CDR P))
                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (P) (NUM (DIP2A (CADR (CDDR P)))))
                                 (CAR P))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ O (APPLY1 'TORDER (LIST O)))
      (RETURN (CONS 'LIST (APPEND CL (APPEND R CR)))))) 
(FLAG '(NC_FACTORIZE) 'OPFN) 
(PUT 'NC_COMMFACTORS 'NUMBER-OF-ARGS 1) 
(PUT 'NC_COMMFACTORS 'DEFINED-ON-LINE '55) 
(PUT 'NC_COMMFACTORS 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_COMMFACTORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NC_COMMFACTORS (U)
    (PROG (O *GSUGAR COMM CR CL)
      (SETQ O (APPLY1 'TORDER '(GRADLEX)))
      (NC-GSETUP)
      (SETQ COMM (NC_COMMFACTORS* U))
      (SETQ CL (CAR COMM))
      (SETQ U (CADR COMM))
      (SETQ CR (CADDR COMM))
      (SETQ O (APPLY1 'TORDER (LIST O)))
      (RETURN (LIST 'LIST (CONS 'LIST CL) U (CONS 'LIST CR))))) 
(FLAG '(NC_COMMFACTORS) 'OPFN) 
(PUT 'NC_COMMFACTORS* 'NUMBER-OF-ARGS 1) 
(PUT 'NC_COMMFACTORS* 'DEFINED-ON-LINE '66) 
(PUT 'NC_COMMFACTORS* 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_COMMFACTORS* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NC_COMMFACTORS* (U)
    ((LAMBDA (RIGHT)
       (PROG (F FF UU COMM L CRL CLL *NCG-RIGHT W)
         (SETQ UU (SUBLIS NCPI-NAMES* (CAR (SIMP U))))
         ((LAMBDA (NCMP*) (SETQ COMM (FCTRF (REORDER UU)))) NIL)
         (COND
          ((AND (NULL (CDDR COMM)) (EQUAL (CDADR COMM) 1))
           (PROGN
            (COND (*TRNC (WRITEPRI "no commutative factors found" 'ONLY)))
            (GO NO_COMM))))
         (SETQ L
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F (CDR COMM))
                  STARTOVER
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (F)
                              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                (SETQ I 1)
                                (COND
                                 ((MINUSP (DIFFERENCE (CDR F) I))
                                  (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 (REVAL1 (PREPF (CAR F)) T)
                                                 NIL)))
                               LOOPLABEL
                                (SETQ I (PLUS2 I 1))
                                (COND
                                 ((MINUSP (DIFFERENCE (CDR F) I))
                                  (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS (REVAL1 (PREPF (CAR F)) T) NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                            (CAR F)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ F (CDR F))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (F)
                              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                                (SETQ I 1)
                                (COND
                                 ((MINUSP (DIFFERENCE (CDR F) I))
                                  (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 (REVAL1 (PREPF (CAR F)) T)
                                                 NIL)))
                               LOOPLABEL
                                (SETQ I (PLUS2 I 1))
                                (COND
                                 ((MINUSP (DIFFERENCE (CDR F) I))
                                  (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS (REVAL1 (PREPF (CAR F)) T) NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL)))
                            (CAR F)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ F (CDR F))
                   (GO LOOPLABEL)))
         (COND (*TRNC (WRITEPRI "testing commutative factors:" 'ONLY)))
         (SETQ UU (A2NCVDP U))
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (SETQ F (CAR L))
            (SETQ L (CDR L))
            (COND (*TRNC (WRITEPRI (MKQUOTE F) 'FIRST)))
            (SETQ *NCG-RIGHT RIGHT)
            (COND
             (((LAMBDA (U) (OR (NULL U) (NULL (CADR (CDDR U)))))
               (CDR (SETQ W (NC-QREMF UU (SETQ FF (A2NCVDP F))))))
              (PROGN
               (COND (*TRNC (WRITEPRI (NC_DIR) 'LAST)))
               (SETQ CLL (APPEND CLL (LIST F)))
               (SETQ UU (CAR W))))
             (((LAMBDA (U) (OR (NULL U) (NULL (CADR (CDDR U)))))
               (CDR
                (PROGN
                 (SETQ *NCG-RIGHT (NOT RIGHT))
                 (SETQ W (NC-QREMF UU FF)))))
              (PROGN
               (COND (*TRNC (WRITEPRI (NC_DIR) 'LAST)))
               (SETQ CRL (CONS F CRL))
               (SETQ UU (CAR W))))
             (*TRNC (WRITEPRI " -- discarded" 'LAST))))
           (GO WHILELABEL))
         (COND ((AND (NULL CRL) (NULL CLL)) (GO NO_COMM)))
         (SETQ U (DIP2A (CADR (CDDR UU))))
         (COND
          (*TRNC
           (PROGN
            (WRITEPRI "remaining noncom  part:" 'FIRST)
            (WRITEPRI (MKQUOTE U) 'LAST))))
        NO_COMM
         (RETURN (LIST CRL U CLL))))
     *NCG-RIGHT)) 
(PUT 'NC_DIR 'NUMBER-OF-ARGS 0) 
(PUT 'NC_DIR 'DEFINED-ON-LINE '94) 
(PUT 'NC_DIR 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_DIR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NC_DIR NIL (COND (*NCG-RIGHT " right") (T " left"))) 
(PUT 'ONESIDE-FACTOR 'NUMBER-OF-ARGS 3) 
(PUT 'ONESIDE-FACTOR 'DEFINED-ON-LINE '96) 
(PUT 'ONESIDE-FACTOR 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'ONESIDE-FACTOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ONESIDE-FACTOR (W M ALL)
    (PROG (U D R MX O *GSUGAR)
      (SETQ D (SETQ R 0))
      (SETQ U (REVAL1 (CAR W) T))
      (COND
       ((CDR W)
        (PROGN
         (SETQ D (REVAL1 (CAR (SETQ W (CDR W))) T))
         (COND ((CDR W) (SETQ R (REVAL1 (CADR W) T)))))))
      (SETQ O (APPLY1 'TORDER '(GRADLEX)))
      (NC-GSETUP)
      (COND ((OR (EQUAL R 0) (EQUAL R '(LIST))) (SETQ R NIL))
            (T
             (PROGN
              (SETQ R (CDR (LISTEVAL R NIL)))
              (SETQ R
                      (CADR
                       (A2VDP
                        (COND ((NULL (CDR R)) (REVAL1 (CAR R) T))
                              (T
                               (CONS 'TIMES
                                     (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                       (SETQ Y R)
                                       (COND ((NULL Y) (RETURN NIL)))
                                       (SETQ FORALL-RESULT
                                               (SETQ FORALL-ENDPTR
                                                       (CONS
                                                        ((LAMBDA (Y)
                                                           (REVAL1 Y T))
                                                         (CAR Y))
                                                        NIL)))
                                      LOOPLABEL
                                       (SETQ Y (CDR Y))
                                       (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                       (RPLACD FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (Y) (REVAL1 Y T))
                                                 (CAR Y))
                                                NIL))
                                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                       (GO LOOPLABEL)))))))))))
      (SETQ D (REVAL1 D T))
      (COND ((EQUAL D 0) (SETQ D 1000))
            ((NOT (FIXP D)) (PROGN (SETQ MX (CADR (A2VDP D))) (SETQ D 1000))))
      (SETQ R (NC_FACTORIZE0 (A2NCVDP U) M D R MX ALL))
      (SETQ O (APPLY1 'TORDER (LIST O)))
      (RETURN
       (PROG (W FORALL-RESULT FORALL-ENDPTR)
         (SETQ W R)
         (COND ((NULL W) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (W) (NUM (DIP2A (CADR (CDDR W))))) (CAR W))
                          NIL)))
        LOOPLABEL
         (SETQ W (CDR W))
         (COND ((NULL W) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (W) (NUM (DIP2A (CADR (CDDR W))))) (CAR W))
                       NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'LEFT_FACTOR 'PSOPFN
     (FUNCTION
      (LAMBDA (W)
        (PROGN (SETQ W (OR (ONESIDE-FACTOR W 'R NIL) W)) (REVAL1 (CAR W) T))))) 
(PUT 'LEFT_FACTORS 'PSOPFN
     (FUNCTION (LAMBDA (W) (CONS 'LIST (ONESIDE-FACTOR W 'R T))))) 
(PUT 'RIGHT_FACTOR 'PSOPFN
     (FUNCTION
      (LAMBDA (W)
        (PROGN (SETQ W (OR (ONESIDE-FACTOR W 'L NIL) W)) (REVAL1 (CAR W) T))))) 
(PUT 'RIGHT_FACTORS 'PSOPFN
     (FUNCTION (LAMBDA (W) (CONS 'LIST (ONESIDE-FACTOR W 'L T))))) 
(PUT 'NC_FACTORIZE_ALL 'NUMBER-OF-ARGS 1) 
(FLAG '(NC_FACTORIZE_ALL) 'OPFN) 
(PUT 'NC_FACTORIZE_ALL 'DEFINED-ON-LINE '129) 
(PUT 'NC_FACTORIZE_ALL 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE_ALL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NC_FACTORIZE_ALL (U)
    (PROG (*NCG-RIGHT D F W WN Q R TRNC NC_FACTOR_TIME*)
      (SETQ NC_FACTOR_TIME* (AEVAL (TIME)))
      (SETQ TRNC (AEVAL *TRNC))
      (SETQ *TRNC NIL)
      (SETQ W (AEVAL (LIST 'LIST (LIST 'LIST U))))
      (SETQ R (AEVAL (LIST 'LIST)))
      (SETQ *NCG-RIGHT NIL)
     LOOP
      (COND ((EVALEQUAL (AEVAL W) (AEVAL (LIST 'LIST))) (GO DONE)))
      (SETQ WN '(LIST))
      (PROG (C)
        (SETQ C (GETRLIST (AEVAL W)))
       LAB
        (COND ((NULL C) (RETURN NIL)))
        ((LAMBDA (C)
           (PROGN
            (SETQ Q (CADR C))
            (SETQ F (AEVAL (LIST 'RIGHT_FACTORS Q (LIST 'LIST) (LIST 'LIST))))
            (COND
             ((BOOLVALUE* TRNC)
              (PROGN
               (ASSGNPRI (AEVAL "ncfctrall: Right factors of (") NIL 'FIRST)
               (ASSGNPRI (AEVAL Q) NIL NIL)
               (ASSGNPRI (AEVAL "): ") NIL NIL)
               (ASSGNPRI (AEVAL F) NIL 'LAST))))
            (COND
             ((EVALEQUAL (AEVAL F) (AEVAL (LIST 'LIST)))
              (SETQ R (AEVAL (LIST 'CONS C R)))))
            (PROG (FC)
              (SETQ FC (GETRLIST (AEVAL F)))
             LAB
              (COND ((NULL FC) (RETURN NIL)))
              ((LAMBDA (FC)
                 (PROGN
                  (SETQ D (AEVAL (LIST 'NC_DIVIDE Q FC)))
                  (COND
                   ((BOOLVALUE* TRNC)
                    (PROGN
                     (ASSGNPRI (AEVAL "ncfctrall: Quotient (") NIL 'FIRST)
                     (ASSGNPRI (AEVAL Q) NIL NIL)
                     (ASSGNPRI (AEVAL ") / (") NIL NIL)
                     (ASSGNPRI (AEVAL FC) NIL NIL)
                     (ASSGNPRI (AEVAL "): ") NIL NIL)
                     (ASSGNPRI (AEVAL D) NIL 'LAST))))
                  (SETQ WN
                          (AEVAL
                           (LIST 'CONS
                                 (LIST 'CONS (LIST 'FIRST D)
                                       (LIST 'CONS FC (LIST 'REST C)))
                                 WN)))))
               (CAR FC))
              (SETQ FC (CDR FC))
              (GO LAB))))
         (CAR C))
        (SETQ C (CDR C))
        (GO LAB))
      (SETQ W (AEVAL WN))
      (GO LOOP)
     DONE
      (SETQ *TRNC TRNC)
      (RETURN (AEVAL R)))) 
(PUT 'NC_FACTORIZE0 'NUMBER-OF-ARGS 6) 
(PUT 'NC_FACTORIZE0 'DEFINED-ON-LINE '151) 
(PUT 'NC_FACTORIZE0 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NC_FACTORIZE0 (U M D RS MX ALL)
    ((LAMBDA (NC_FACTOR_TIME*)
       (PROGN
        (COND ((NOT (NUMBERP NC_FACTOR_TIME*)) (SETQ NC_FACTOR_TIME* (TIME))))
        (NC_FACTORIZE1 U M D RS MX ALL)))
     NC_FACTOR_TIME*)) 
(PUT 'NC_FACTORIZE1 'NUMBER-OF-ARGS 6) 
(PUT 'NC_FACTORIZE1 'DEFINED-ON-LINE '155) 
(PUT 'NC_FACTORIZE1 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NC_FACTORIZE1 (U M D RS MX ALL)
    (PROG (EV EVL EVLX F FF *NCG-RIGHT)
      (NC_FACTORIZE_TIMECHECK)
      (SETQ MX
              (COND
               ((NULL MX)
                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Y VDPVARS*)
                  (COND ((NULL Y) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (Y) 1000) (CAR Y)) NIL)))
                 LOOPLABEL
                  (SETQ Y (CDR Y))
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (Y) 1000) (CAR Y)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               (T
                (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                  (SETQ Y MX)
                  (COND ((NULL Y) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (Y)
                                      (COND ((GREATERP Y 0) Y) (T 1000)))
                                    (CAR Y))
                                   NIL)))
                 LOOPLABEL
                  (SETQ Y (CDR Y))
                  (COND ((NULL Y) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (Y) (COND ((GREATERP Y 0) Y) (T 1000)))
                            (CAR Y))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND (*TRNC (PROGN (PRIN2 "factorize ") (VDPPRINT U))))
      (SETQ EV (CADR U))
      (COND
       ((OR (NULL EV) (AND (EQUAL (CAR EV) 0) (VEVZERO?1 (CDR EV))))
        (RETURN (LIST U))))
      (SETQ D (OR D (QUOTIENT (VEVTDEG EV) 2)))
      (SETQ EVLX
              (SORT (NC_FACTORIZE1-EVL EV)
                    (FUNCTION (LAMBDA (X Y) (LESSP (EVCOMP X Y) 0)))))
      (COND ((EQUAL M 'R) (GO R)))
      (SETQ EVL EVLX)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (OR (NULL F) ALL) EVL (LEQ (VEVTDEG (CAR EVL)) D)))
          (RETURN NIL)))
        (PROGN
         (COND
          ((AND
            (NOT
             (OR (NULL (CAR EVL))
                 (AND (EQUAL (CAR (CAR EVL)) 0) (VEVZERO?1 (CDR (CAR EVL))))))
            (NEQ (CAR EVL) EV) (OR (NULL RS) (VEVMTEST? (CAR EVL) RS))
            (VEVMTEST? MX (CAR EVL)))
           (SETQ F (APPEND F (NC_FACTORIZE2 U (CAR EVL) RS MX ALL)))))
         (SETQ EVL (CDR EVL)))
        (GO WHILELABEL))
      (COND ((OR F (EQUAL M 'L)) (GO C)))
      (SETQ D (DIFFERENCE (VEVTDEG EV) D))
     R
      (SETQ *NCG-RIGHT T)
      (SETQ EVL EVLX)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (OR (NULL F) ALL) EVL (LEQ (VEVTDEG (CAR EVL)) D)))
          (RETURN NIL)))
        (PROGN
         (COND
          ((AND
            (NOT
             (OR (NULL (CAR EVL))
                 (AND (EQUAL (CAR (CAR EVL)) 0) (VEVZERO?1 (CDR (CAR EVL))))))
            (NEQ (CAR EVL) EV) (OR (NULL RS) (VEVMTEST? (CAR EVL) RS))
            (VEVMTEST? MX (CAR EVL)))
           (SETQ F (APPEND F (NC_FACTORIZE2 U (CAR EVL) RS MX ALL)))))
         (SETQ EVL (CDR EVL)))
        (GO WHILELABEL))
     C
      (COND ((NULL F) (RETURN (COND (M NIL) (T (LIST U))))))
      (COND (ALL (RETURN F)))
      (COND (M (RETURN (LIST (CDR F)))))
      (SETQ FF (NC_FACTORIZE1 (CAR F) NIL NIL NIL MX ALL))
      (RETURN
       (COND (*NCG-RIGHT (APPEND (LIST (CDR F)) FF))
             (T (APPEND FF (LIST (CDR F)))))))) 
(PUT 'NC_FACTORIZE1-EVL 'NUMBER-OF-ARGS 1) 
(PUT 'NC_FACTORIZE1-EVL 'DEFINED-ON-LINE '205) 
(PUT 'NC_FACTORIZE1-EVL 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE1-EVL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NC_FACTORIZE1-EVL (U)
    (COND ((NULL U) '(NIL))
          (T
           ((LAMBDA (W)
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 0)
               STARTOVER
                (COND ((MINUSP (DIFFERENCE (CAR U) I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (PROG (E FORALL-RESULT FORALL-ENDPTR)
                          (SETQ E W)
                          (COND ((NULL E) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (E) (CONS I E)) (CAR E))
                                           NIL)))
                         LOOPLABEL
                          (SETQ E (CDR E))
                          (COND ((NULL E) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (E) (CONS I E)) (CAR E)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ I (PLUS2 I 1))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((MINUSP (DIFFERENCE (CAR U) I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (PROG (E FORALL-RESULT FORALL-ENDPTR)
                          (SETQ E W)
                          (COND ((NULL E) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (E) (CONS I E)) (CAR E))
                                           NIL)))
                         LOOPLABEL
                          (SETQ E (CDR E))
                          (COND ((NULL E) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (E) (CONS I E)) (CAR E)) NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ I (PLUS2 I 1))
                (GO LOOPLABEL)))
            (NC_FACTORIZE1-EVL (CDR U)))))) 
(AEVAL (OPERATOR (LIST 'NCC@))) 
(PUT 'NC_FACTORIZE2 'NUMBER-OF-ARGS 5) 
(PUT 'NC_FACTORIZE2 'DEFINED-ON-LINE '213) 
(PUT 'NC_FACTORIZE2 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE NC_FACTORIZE2 (U EV RS MX ALL)
    (PROG (AR P Q VL R S SO SOL W Y N *BCSUBS2)
      (SETQ N 0)
      (NC_FACTORIZE_TIMECHECK)
      (SETQ P (A2DIP 0))
      (COND
       (*TRNC
        (PROGN
         (PRIN2 (COND (*NCG-RIGHT "right ") (T "left ")))
         (PRIN2 "Ansatz for leading term > ")
         (VDPPRIN2 (VDPFMON (A2BC 1) EV))
         (PRIN2 " < time so far:")
         (PRIN2 (DIFFERENCE (TIME) NC_FACTOR_TIME*))
         (PRIN2T "ms"))))
      (PROG (E)
        (SETQ E (NC_FACTORIZE2EVL EV RS MX))
       LAB
        (COND ((NULL E) (RETURN NIL)))
        ((LAMBDA (E)
           (PROGN
            (SETQ Q (LIST 'NCC@ (SETQ N (PLUS N 1))))
            (SETQ P (DIPSUM P (CONS E (CONS (A2BC Q) NIL))))))
         (CAR E))
        (SETQ E (CDR E))
        (GO LAB))
      (SETQ W P)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL W))) (RETURN NIL)))
        (PROGN (SETQ VL (CONS (BC2A (CADR W)) VL)) (SETQ W (CDDR W)))
        (GO WHILELABEL))
      (SETQ VL (REVERSIP VL))
      (SETQ P (DIP2VDP P))
      (SETQ R (NC-NORMALFORM U (LIST P) NIL NIL))
      (NC_FACTORIZE_TIMECHECK)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (OR (NULL R) (NULL (CADR (CDDR R)))))) (RETURN NIL)))
        (PROGN (SETQ S (CONS (BC2A (CADDR R)) S)) (SETQ R (VDPRED R)))
        (GO WHILELABEL))
      (COND
       (*TRNC
        (PROGN
         (PRIN2T "internal equation system:")
         (WRITEPRI (MKQUOTE (CONS 'LIST S)) 'ONLY))))
      (PROG (V)
        (SETQ V VL)
       LAB
        (COND ((NULL V) (RETURN NIL)))
        ((LAMBDA (V) (COND ((NOT (SMEMBER V S)) (SETQ SO V)))) (CAR V))
        (SETQ V (CDR V))
        (GO LAB))
      (COND ((AND *TRNC SO) (PROGN (PRIN2 "free:") (PRIN2T SO))))
      (COND
       (SO
        (SETQ SOL
                (LIST
                 (CONS (CONS SO 1)
                       (PROG (V FORALL-RESULT FORALL-ENDPTR)
                         (SETQ V VL)
                         (COND ((NULL V) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (V) (CONS V 0)) (CAR V))
                                          NIL)))
                        LOOPLABEL
                         (SETQ V (CDR V))
                         (COND ((NULL V) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (V) (CONS V 0)) (CAR V)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL)))))))
      (COND
       ((OR (NULL SOL) ALL) (SETQ SOL (APPEND SOL (NC_FACTSOLVE S VL ALL)))))
      (COND
       ((NULL SOL)
        (PROGN
         (COND
          (*TRNC
           (PROGN
            (PRIN2T "no internal solutions")
            (PRIN2T "====================================="))))
         (RETURN NIL))))
      (COND
       (*TRNC
        (PROGN
         (PRIN2T "internal solutions:")
         (PROG (S)
           (SETQ S SOL)
          LAB
           (COND ((NULL S) (RETURN NIL)))
           ((LAMBDA (S)
              (PROGN
               (PROG (Q)
                 (SETQ Q S)
                LAB
                 (COND ((NULL Q) (RETURN NIL)))
                 ((LAMBDA (Q)
                    (PROGN
                     (WRITEPRI (MKQUOTE (CAR Q)) 'FIRST)
                     (WRITEPRI (MKQUOTE " = ") NIL)
                     (WRITEPRI (MKQUOTE (CDR Q)) 'LAST)))
                  (CAR Q))
                 (SETQ Q (CDR Q))
                 (GO LAB))
               (PRIN2T "=====================================")))
            (CAR S))
           (SETQ S (CDR S))
           (GO LAB))
         NIL)))
     COLL
      (NC_FACTORIZE_TIMECHECK)
      (SETQ SO (CAR SOL))
      (SETQ SOL (CDR SOL))
      (SETQ Y (DIP2VDP (DIPPOLISH (DIPSUBF SO (CADR (CDDR P))))))
      (COND
       ((OR (NULL (CADR Y))
            (AND (EQUAL (CAR (CADR Y)) 0) (VEVZERO?1 (CDR (CADR Y)))))
        (COND ((NOT ALL) (RETURN NIL)) (SOL (GO COLL)) (T (GO DONE_ALL)))))
      (COND
       ((OR (SMEMQ 'EXPT Y) (SMEMQ 'SQRT Y) (SMEMQ 'ROOT_OF Y))
        (SETQ *BCSUBS2 T)))
      (SETQ W (NC-QREMF U Y))
      (COND
       ((NOT (OR (NULL (CDR W)) (NULL (CADR (CDDR (CDR W))))))
        (PROGN
         (PRIN2 "division failure")
         (VDPPRINT U)
         (PRIN2T "/")
         (VDPPRINT Y)
         (PRIN2 "=> ")
         (VDPPRINT (CAR W))
         (PRIN2 "rem: ")
         (VDPPRINT (CDR W))
         (REDERR "noncom factorize"))))
      (COND
       (*TRNC
        (PROGN
         (TERPRI)
         (PRIN2 "splitting into > ")
         (VDPPRIN2 (CAR W))
         (PRIN2T " < and")
         (PRIN2 " > ")
         (VDPPRIN2 Y)
         (PRIN2T " <")
         (TERPRI))))
      (SETQ AR (CONS Y AR))
      (COND (ALL (COND (SOL (GO COLL)) (T (GO DONE_ALL)))))
     DONE_ONE
      (RETURN (CONS (CAR W) Y))
     DONE_ALL
      (RETURN AR))) 
(PUT 'NC_FACTSOLVE 'NUMBER-OF-ARGS 3) 
(PUT 'NC_FACTSOLVE 'DEFINED-ON-LINE '290) 
(PUT 'NC_FACTSOLVE 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTSOLVE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NC_FACTSOLVE (S VL ALL)
    (PROG (V SB NS SO SOA SOL NZ W Q Z R ABORT)
      (SETQ V (CAR (SIMP (CAR VL))))
      (SETQ NS
              (PROG (E FORALL-RESULT FORALL-ENDPTR)
                (SETQ E S)
                (COND ((NULL E) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (E) (CAR (SIMP E))) (CAR E))
                                      NIL)))
               LOOPLABEL
                (SETQ E (CDR E))
                (COND ((NULL E) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (E) (CAR (SIMP E))) (CAR E)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ R T)
      (PROG ()
       WHILELABEL
        (COND ((NOT R) (RETURN NIL)))
        (PROGN
         (SETQ R NIL)
         (SETQ S NS)
         (SETQ NS NIL)
         (PROG (E)
           (SETQ E S)
          LAB
           (COND ((NULL E) (RETURN NIL)))
           ((LAMBDA (E)
              (COND
               ((NOT ABORT)
                (PROGN
                 (SETQ E (ABSF (CAR (SUBF E SB))))
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT (SETQ Q ((LAMBDA (*EXP) (QUOTF1 E V)) T)))
                     (RETURN NIL)))
                   (SETQ E Q)
                   (GO WHILELABEL))
                 (COND ((NULL E) NIL)
                       ((OR (OR (ATOM E) (ATOM (CAR E)))
                            (NOT (MEMBER (CAAAR E) VL)))
                        (SETQ ABORT T))
                       ((AND (NULL (CDR E))
                             (OR (ATOM (CDAR E)) (ATOM (CAR (CDAR E)))))
                        (PROGN
                         (SETQ W (CAAAR E))
                         (SETQ SB (CONS (CONS W 0) SB))
                         (SETQ R T)
                         (SETQ VL (DELETE W VL))))
                       ((NOT (MEMBER E NS)) (SETQ NS (CONS E NS))))))))
            (CAR E))
           (SETQ E (CDR E))
           (GO LAB)))
        (GO WHILELABEL))
      (COND ((OR ABORT (NULL VL)) (RETURN NIL)))
      (NC_FACTORIZE_TIMECHECK)
      (COND
       ((AND (NULL NS) VL)
        (PROGN
         (SETQ SOL
                 (LIST
                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                    (SETQ X VL)
                    (COND ((NULL X) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (X) (CONS X 1)) (CAR X))
                                          NIL)))
                   LOOPLABEL
                    (SETQ X (CDR X))
                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (X) (CONS X 1)) (CAR X)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))
         (GO DONE))))
      (SETQ S
              (PROG (E FORALL-RESULT FORALL-ENDPTR)
                (SETQ E NS)
                (COND ((NULL E) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (E) (PREPF E)) (CAR E)) NIL)))
               LOOPLABEL
                (SETQ E (CDR E))
                (COND ((NULL E) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (E) (PREPF E)) (CAR E)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*TRNC
        (PROGN
         (PRIN2 "solving ")
         (PRIN2 (LENGTH S))
         (PRIN2 " polynomial equations for ")
         (PRIN2 (LENGTH VL))
         (PRIN2T " variables")
         (PROG (E)
           (SETQ E S)
          LAB
           (COND ((NULL E) (RETURN NIL)))
           ((LAMBDA (E) (WRITEPRI (MKQUOTE E) 'ONLY)) (CAR E))
           (SETQ E (CDR E))
           (GO LAB)))))
      (SETQ W
              ((LAMBDA (DIPVARS*)
                 (CDR (SOLVEEVAL (LIST (CONS 'LIST S) (CONS 'LIST VL)))))
               NIL))
      (COND
       (*TRNC
        (PROGN
         (PRIN2 "solve returned")
         (PROG (SL)
           (SETQ SL W)
          LAB
           (COND ((NULL SL) (RETURN NIL)))
           ((LAMBDA (SL) (WRITEPRI (MKQUOTE SL) 'ONLY)) (CAR SL))
           (SETQ SL (CDR SL))
           (GO LAB)))))
     LOOP
      (NC_FACTORIZE_TIMECHECK)
      (COND ((NULL W) (GO DONE)))
      (SETQ SO (CDR (CAR W)))
      (SETQ W (CDR W))
      (SETQ SOA NIL)
      (COND ((AND (SMEMQ 'I SO) (NULL *COMPLEX)) (GO LOOP)))
      (PROG (Y)
        (SETQ Y VL)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (COND
            ((NOT (SMEMBER Y SO))
             (PROGN (SETQ SOA (CONS (CONS Y 1) SOA)) (SETQ NZ T)))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (PROG (Y)
        (SETQ Y SO)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROGN
            (SETQ Z (NC_FACTORIZE_UNWRAP (REVAL1 (CADDR Y) T) SOA))
            (SETQ NZ (OR NZ (NEQ Z 0)))
            (SETQ SOA (CONS (CONS (CADR Y) Z) SOA))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (COND ((NOT NZ) (GO LOOP)))
      (SETQ Q (ASSOC (CAR VL) SOA))
      (COND ((OR (NULL Q) (EQUAL (CDR Q) 0)) (GO LOOP)))
      (SETQ SOA
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J SOA)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (CONS (CAR J) (SUBLIS SOA (CDR J))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (CONS (CAR J) (SUBLIS SOA (CDR J))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SOL (CONS SOA SOL))
      (COND (ALL (GO LOOP)))
     DONE
      (SETQ SOL
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S SOL)
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (S) (APPEND SB S)) (CAR S))
                                      NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (S) (APPEND SB S)) (CAR S)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       (*TRNC
        (COND
         ((NULL SOL)
          (PROGN (PRIN2T "no solutions") (PRIN2T "-------------------------")))
         (T
          (PROGN
           (PRIN2T "solutions:")
           (PROG (W)
             (SETQ W SOL)
            LAB
             (COND ((NULL W) (RETURN NIL)))
             ((LAMBDA (W)
                (WRITEPRI
                 (MKQUOTE
                  (CONS 'LIST
                        (PROG (S FORALL-RESULT FORALL-ENDPTR)
                          (SETQ S W)
                          (COND ((NULL S) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (S)
                                              (LIST 'EQUAL (CAR S) (CDR S)))
                                            (CAR S))
                                           NIL)))
                         LOOPLABEL
                          (SETQ S (CDR S))
                          (COND ((NULL S) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (S) (LIST 'EQUAL (CAR S) (CDR S)))
                                    (CAR S))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL))))
                 'ONLY))
              (CAR W))
             (SETQ W (CDR W))
             (GO LAB))
           (PRIN2T "-------------------------"))))))
      (RETURN SOL))) 
(PUT 'DIPSUBF 'NUMBER-OF-ARGS 2) 
(PUT 'DIPSUBF 'DEFINED-ON-LINE '361) 
(PUT 'DIPSUBF 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'DIPSUBF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DIPSUBF (A U)
    (COND ((NULL U) NIL)
          (T
           ((LAMBDA (Q R)
              (PROGN
               (SETQ Q (COND (Q (CDR Q)) (T (CADR U))))
               (COND ((NEQ Q 0) (CONS (CAR U) (CONS (A2BC Q) R))) (T R))))
            (ASSOC (BC2A (CADR U)) A) (DIPSUBF A (CDDR U)))))) 
(PUT 'DIPPOLISH 'NUMBER-OF-ARGS 1) 
(PUT 'DIPPOLISH 'DEFINED-ON-LINE '368) 
(PUT 'DIPPOLISH 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'DIPPOLISH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIPPOLISH (P1) (DIPRECTOINT P1 (DIPLCM P1))) 
(PUT 'NC_FACTORIZE_UNWRAP 'NUMBER-OF-ARGS 2) 
(PUT 'NC_FACTORIZE_UNWRAP 'DEFINED-ON-LINE '370) 
(PUT 'NC_FACTORIZE_UNWRAP 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE_UNWRAP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NC_FACTORIZE_UNWRAP (U S)
    (COND ((ATOM U) U) ((EQCAR U 'ARBCOMPLEX) 1)
          (T
           ((LAMBDA (Q)
              (COND (Q (CDR Q))
                    (T
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X U)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X) (NC_FACTORIZE_UNWRAP X S))
                                         (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (X) (NC_FACTORIZE_UNWRAP X S))
                                 (CAR X))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
            (ASSOC U S))))) 
(PUT 'NC_FACTORIZE2EVL 'NUMBER-OF-ARGS 3) 
(PUT 'NC_FACTORIZE2EVL 'DEFINED-ON-LINE '376) 
(PUT 'NC_FACTORIZE2EVL 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE2EVL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NC_FACTORIZE2EVL (EV RS MX)
    (PROG (Q FORALL-RESULT FORALL-ENDPTR)
      (SETQ Q
              (NC_FACTORIZE2-EVL1 (MIN (EVTDEG MX) (EVTDEG EV)) (LENGTH EV)
               RS))
     STARTOVER
      (COND ((NULL Q) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (Q)
                 (COND
                  ((AND (NOT (EQUAL 1 (EVCOMP Q EV))) (VEVMTEST? MX Q))
                   (LIST Q))))
               (CAR Q)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ Q (CDR Q))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL Q) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (Q)
                 (COND
                  ((AND (NOT (EQUAL 1 (EVCOMP Q EV))) (VEVMTEST? MX Q))
                   (LIST Q))))
               (CAR Q)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ Q (CDR Q))
      (GO LOOPLABEL))) 
(PUT 'NC_FACTORIZE2-EVL1 'NUMBER-OF-ARGS 3) 
(PUT 'NC_FACTORIZE2-EVL1 'DEFINED-ON-LINE '383) 
(PUT 'NC_FACTORIZE2-EVL1 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE2-EVL1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE NC_FACTORIZE2-EVL1 (N M RS)
    (COND ((EQUAL M 0) '(NIL))
          (T
           (PROG (I FORALL-RESULT FORALL-ENDPTR)
             (SETQ I 0)
            STARTOVER
             (COND
              ((MINUSP
                (DIFFERENCE
                 (COND ((OR (NULL RS) (GREATERP (CAR RS) 0)) N) (T 0)) I))
               (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (PROG (E FORALL-RESULT FORALL-ENDPTR)
                       (SETQ E
                               (NC_FACTORIZE2-EVL1 (IDIFFERENCE N I)
                                (IDIFFERENCE M 1) (COND (RS (CDR RS)))))
                       (COND ((NULL E) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (E) (CONS I E)) (CAR E))
                                             NIL)))
                      LOOPLABEL
                       (SETQ E (CDR E))
                       (COND ((NULL E) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (E) (CONS I E)) (CAR E)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ I (PLUS2 I 1))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND
              ((MINUSP
                (DIFFERENCE
                 (COND ((OR (NULL RS) (GREATERP (CAR RS) 0)) N) (T 0)) I))
               (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (PROG (E FORALL-RESULT FORALL-ENDPTR)
                       (SETQ E
                               (NC_FACTORIZE2-EVL1 (IDIFFERENCE N I)
                                (IDIFFERENCE M 1) (COND (RS (CDR RS)))))
                       (COND ((NULL E) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (E) (CONS I E)) (CAR E))
                                             NIL)))
                      LOOPLABEL
                       (SETQ E (CDR E))
                       (COND ((NULL E) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (E) (CONS I E)) (CAR E)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ I (PLUS2 I 1))
             (GO LOOPLABEL))))) 
(PUT 'NC_FACTORIZE_TIMECHECK 'NUMBER-OF-ARGS 0) 
(PUT 'NC_FACTORIZE_TIMECHECK 'DEFINED-ON-LINE '390) 
(PUT 'NC_FACTORIZE_TIMECHECK 'DEFINED-IN-FILE 'NCPOLY/NCFACTOR.RED) 
(PUT 'NC_FACTORIZE_TIMECHECK 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NC_FACTORIZE_TIMECHECK NIL
    (COND
     ((AND (FIXP NC_FACTOR_TIME) (GREATERP NC_FACTOR_TIME 0)
           (GREATERP (DIFFERENCE (TIME) NC_FACTOR_TIME*) NC_FACTOR_TIME))
      (REDERR "time overflow in noncom. factorization")))) 
(ENDMODULE) 