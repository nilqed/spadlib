(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYSIMP)) 
(EXPORTS (LIST 'TAYSIMPP 'TAYSIMPSQ 'TAYSIMPSQ* 'EXPTTAYRAT 'EXPTTAYRAT1)) 
(IMPORTS
 (LIST '*F2Q '*K2Q '*P2F '*P2Q '*T2Q 'ADDSQ 'APPLY1 'DENR 'DOMAINP 'EVENP
       'EXPTSQ 'INVSQ 'KERNP 'MK*SQ 'MKRN 'MULTF 'MULTPQ 'MULTSQ 'MVAR 'NTH
       'NUMR 'OVER 'PDEG 'PREPSQ 'QUOTSQ 'REVERSIP 'SFP 'SIMP 'SIMP* 'TC 'TO
       'TPOW '*Q2TAYEXP '*TAY2F '*TAY2Q '*TAYEXP2Q 'COMP.TP.-P 'CST-TAYLOR*
       'HAS-TAYLOR* 'FIND-NON-ZERO 'GET-DEGREELIST 'HAS-TAYVARS
       'INVERT-POWERLIST 'MAKE-CST-COEFFLIS 'MAKE-CST-POWERLIST 'MAKE-TAYLOR*
       'PRUNE-COEFFLIST 'RESIMPTAYLOR 'TAYCFPL 'TAYCFSQ 'TAYCOEFFLIST 'TAYFLAGS
       'TAYGETCOEFF 'TAYLOR-KERNEL-SQ-P 'TAYLOR*P '|TAYLOR:| 'TAYMAKECOEFF
       'TAYMULTCOEFFS 'TAYORIG 'TAYTEMPLATE 'TAYTPELNEXT 'TAYTPELPOINT
       'TAYTPELVARS 'TPNEXTLIST 'CONFUSION 'TAYLOR-ERROR 'TAYLOR-ERROR*
       'ADDTO-ALL-TAYTPELORDERS 'GET-CST-COEFF 'SMALLEST-INCREMENT
       'TAYLOR*-NZCONSTANTP 'TAYLOR*-ZEROP 'TAYLOREXPAND 'TAYLOREXPAND-SF
       'ADDTAYLOR 'ADDTAYLOR-AS-SQ 'INVTAYLOR 'MAKECOEFFPAIRS 'MAKECOEFFS0
       'MULTTAYLOR 'MULTTAYLOR-AS-SQ 'MULTTAYLORSQ 'QUOTTAYLOR-AS-SQ)) 
(FLUID '(*TAYLORAUTOEXPAND *TAYLORKEEPORIGINAL)) 
(PUT 'TAYSIMPSQ 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPSQ 'DEFINED-ON-LINE '83) 
(PUT 'TAYSIMPSQ 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'TAYSIMPSQ 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPSQ (U)
    (PROG (NM DD *MCD)
      (SETQ *MCD T)
      (SETQ DD (TAYSIMPF (CDR U)))
      (COND ((NULL (CAR DD)) (TAYLOR-ERROR 'ZERO-DENOM 'TAYSIMPSQ))
            ((AND (KERNP DD) (EQCAR (CAAAR (CAR DD)) 'TAYLOR*))
             (RETURN
              (TAYSIMPF
               ((LAMBDA (G155)
                  (COND (*PHYSOP-LOADED (PHYSOP-MULTF (CAR U) G155))
                        (T (POLY-MULTF (CAR U) G155))))
                (LIST
                 (CONS (GETPOWER (FKERN (INVTAYLOR (CAAAR (CAR DD)))) 1)
                       1)))))))
      (SETQ NM (TAYSIMPF (CAR U)))
      (RETURN
       (COND
        ((AND (KERNP NM) (EQCAR (CAAAR (CAR NM)) 'TAYLOR*))
         (COND
          ((NOT
            (SMEMBERLP
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (CADDR (CAAAR (CAR NM))))
              STARTOVER
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ X (CDR X))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ X (CDR X))
               (GO LOOPLABEL))
             DD))
           (CONS
            (LIST
             (CONS
              (GETPOWER
               (FKERN
                ((LAMBDA (U)
                   (LIST 'TAYLOR*
                         (PROG (CC FORALL-RESULT FORALL-ENDPTR)
                           (SETQ CC (CADR U))
                           (COND ((NULL CC) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (CC)
                                               (CONS (CAR CC)
                                                     (SUBS2
                                                      (RESIMP (CDR CC)))))
                                             (CAR CC))
                                            NIL)))
                          LOOPLABEL
                           (SETQ CC (CDR CC))
                           (COND ((NULL CC) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (CC)
                                       (CONS (CAR CC)
                                             (SUBS2 (RESIMP (CDR CC)))))
                                     (CAR CC))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         (CADDR U)
                         (COND
                          ((AND *TAYLORKEEPORIGINAL (CADDDR U))
                           (RESIMP (CADDDR U)))
                          (T NIL))
                         (CAR (CDDDDR U))))
                 (MULTTAYLORSQ (CAAAR (CAR NM)) (INVSQ DD))))
               1)
              1))
            1))
          ((TAYLOR*-NZCONSTANTP (CAAAR (CAR NM)))
           (MULTSQ (GET-CST-COEFF (CAAAR (CAR NM))) (INVSQ DD)))
          ((OR (NULL *TAYLORAUTOEXPAND) (SMEMBER 'TAYLOR* DD))
           (MULTSQ NM (INVSQ DD)))
          (T
           (TAYSIMPSQ*
            (QUOTTAYLOR-AS-SQ NM
             (TAYLOREXPAND DD (CADDR (CAAAR (CAR NM)))))))))
        (T (MULTSQ NM (INVSQ DD))))))) 
(PUT 'TAYSIMPSQ* 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPSQ* 'DEFINED-ON-LINE '113) 
(PUT 'TAYSIMPSQ* 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'TAYSIMPSQ* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPSQ* (U) ((LAMBDA (*TAYLORAUTOEXPAND) (TAYSIMPSQ U)) NIL)) 
(PUT 'TAYSIMPF 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPF 'DEFINED-ON-LINE '121) 
(PUT 'TAYSIMPF 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'TAYSIMPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPF (U)
    (PROG (TAY NOTAY X FLG)
      (SETQ NOTAY (CONS NIL 1))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (ATOM U) (ATOM (CAR U))) (SETQ NOTAY (ADDSQ (*D2Q U) NOTAY)))
          ((NOT (SMEMBER 'TAYLOR* (CAR U)))
           (SETQ NOTAY (ADDSQ NOTAY (CONS (LIST (CAR U)) 1))))
          (T
           (PROGN
            (SETQ X (TAYSIMPT (CAR U)))
            (COND
             ((AND (KERNP X) (EQCAR (CAAAR (CAR X)) 'TAYLOR*))
              (COND ((NULL TAY) (SETQ TAY (CAAAR (CAR X))))
                    ((COMP.TP.-P TAY (CAAAR (CAR X)))
                     (SETQ TAY (ADDTAYLOR TAY (CAAAR (CAR X)))))
                    (T (PROGN (SETQ FLG T) (SETQ NOTAY (ADDSQ NOTAY X))))))
             (T (SETQ NOTAY (ADDSQ NOTAY X)))))))
         (SETQ U (COND ((OR (ATOM U) (ATOM (CAR U))) NIL) (T (CDR U)))))
        (GO WHILELABEL))
      (COND
       ((AND (NOT (NULL TAY)) (NOT (NULL (CADDDR TAY)))
             (NULL (CAR (CADDDR TAY))))
        (RETURN NOTAY))
       ((AND (NULL (CAR NOTAY)) (NOT (NULL TAY)))
        (RETURN (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1)))
       ((OR (NULL TAY) (TAYLOR*-ZEROP TAY)) (RETURN NOTAY)))
      (COND
       ((AND (TAYLOR*-NZCONSTANTP TAY) (NOT (SMEMBER 'TAYLOR* NOTAY)))
        (RETURN (ADDSQ (GET-CST-COEFF TAY) NOTAY)))
       ((AND (NULL *TAYLORAUTOEXPAND)
             (SMEMBERLP
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X (CADDR TAY))
               STARTOVER
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ X (CDR X))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ X (CDR X))
                (GO LOOPLABEL))
              NOTAY))
        (RETURN
         (ADDSQ (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1) NOTAY))))
      (COND
       (FLG
        (RETURN
         (ADDSQ (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1) NOTAY)))
       (T
        (PROGN
         (SETQ NOTAY (TAYLOREXPAND NOTAY (CADDR TAY)))
         (RETURN
          (TAYSIMPSQ*
           (ADDTAYLOR-AS-SQ NOTAY
            (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1))))))))) 
(PUT 'TAYSIMPT 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPT 'DEFINED-ON-LINE '184) 
(PUT 'TAYSIMPT 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'TAYSIMPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPT (U)
    (PROG (REST POW)
      (SETQ REST (TAYSIMPF (CDR U)))
      (COND ((NULL (CAR REST)) (RETURN REST)))
      (SETQ POW (CAR U))
      (RETURN
       (COND
        ((NOT (SMEMBER 'TAYLOR* POW))
         (COND
          ((AND (KERNP REST) (EQCAR (CAAAR (CAR REST)) 'TAYLOR*))
           (MULTPOWERINTOTAYLOR POW (CAAAR (CAR REST))))
          (T (MULTSQ (CONS (LIST (CONS POW 1)) 1) REST))))
        (T
         (PROGN
          (SETQ POW (TAYSIMPP POW))
          (COND
           ((AND (NOT (SMEMBER 'TAYLOR* REST))
                 (AND (KERNP POW) (EQCAR (CAAAR (CAR POW)) 'TAYLOR*)))
            (COND
             ((SMEMBERLP
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (CADDR (CAAAR (CAR POW))))
                STARTOVER
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                 (SETQ X (CDR X))
                 (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                LOOPLABEL
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
                 (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                 (SETQ X (CDR X))
                 (GO LOOPLABEL))
               REST)
              (COND
               (*TAYLORAUTOEXPAND
                (TAYSIMPSQ*
                 (MULTTAYLOR-AS-SQ POW
                  (TAYLOREXPAND-SF (CDR U) (CADDR (CAAAR (CAR POW))) NIL))))
               (T (MULTSQ POW REST))))
             (T
              (CONS
               (LIST
                (CONS
                 (GETPOWER (FKERN (MULTTAYLORSQ (CAAAR (CAR POW)) REST)) 1) 1))
               1))))
           (T (MULTTAYLOR-AS-SQ POW REST))))))))) 
(PUT 'MULTPOWERINTOTAYLOR 'NUMBER-OF-ARGS 2) 
(PUT 'MULTPOWERINTOTAYLOR 'DEFINED-ON-LINE '231) 
(PUT 'MULTPOWERINTOTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'MULTPOWERINTOTAYLOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTPOWERINTOTAYLOR (P TK)
    (COND
     ((NOT
       (SMEMBERLP
        (PROG (X FORALL-RESULT FORALL-ENDPTR)
          (SETQ X (CADDR TK))
         STARTOVER
          (COND ((NULL X) (RETURN NIL)))
          (SETQ FORALL-RESULT ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
          (SETQ X (CDR X))
          (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
         LOOPLABEL
          (COND ((NULL X) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR ((LAMBDA (X) (APPEND (CAR X) NIL)) (CAR X)))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
          (SETQ X (CDR X))
          (GO LOOPLABEL))
        P))
      (CONS
       (LIST
        (CONS (GETPOWER (FKERN (MULTTAYLORSQ TK (CONS (LIST (CONS P 1)) 1))) 1)
              1))
       1))
     (*TAYLORAUTOEXPAND
      (TAYSIMPSQ*
       (MULTTAYLOR-AS-SQ (CONS (LIST (CONS (GETPOWER (FKERN TK) 1) 1)) 1)
        (TAYLOREXPAND-SF (LIST (CONS P 1)) (CADDR TK) NIL))))
     ((TAYLOR*-NZCONSTANTP TK)
      (MULTSQ (CONS (LIST (CONS P 1)) 1) (GET-CST-COEFF TK)))
     (T
      (MULTSQ (CONS (LIST (CONS P 1)) 1)
              (CONS (LIST (CONS (GETPOWER (FKERN TK) 1) 1)) 1))))) 
(PUT 'TAYSIMPP 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPP 'DEFINED-ON-LINE '258) 
(PUT 'TAYSIMPP 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'TAYSIMPP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPP (U)
    (COND ((OR (NULL (CAR U)) (NULL (CDR U))) (CONFUSION 'TAYSIMPP))
          ((SFP (CAR U)) (CONS (LIST (CONS U 1)) 1))
          ((NOT (EQCAR (CAR U) 'TAYLOR*))
           ((LAMBDA (X)
              (COND
               ((KERNP X)
                (COND
                 ((EQUAL (SETQ X (CAAAR (CAR X))) (CAR U))
                  (CONS (LIST (CONS U 1)) 1))
                 ((EQUAL (CDR U) 1) (CONS (LIST (CONS (CONS X 1) 1)) 1))
                 (T (TAYSIMPP (CONS X (CDR U))))))
               ((EQUAL (CDR U) 1) X) (T (TAYSIMPSQ (EXPTSQ X (CDR U))))))
            (TAYSIMPMAINVAR (CAR U))))
          ((OR (NOT (FIXP (CDR U))) (EQUAL (CDR U) 0)) (CONFUSION 'TAYSIMPP))
          ((AND (NOT (NULL (CADDDR (CAR U)))) (NULL (CAR (CADDDR (CAR U)))))
           (CONS NIL 1))
          (T
           (CONS
            (LIST
             (CONS
              (GETPOWER
               (FKERN
                (COND ((EQUAL (CDR U) 1) (CAR U))
                      (T (EXPTTAYI (CAR U) (CDR U)))))
               1)
              1))
            1)))) 
(PUT 'TAYSIMPMAINVAR 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPMAINVAR 'DEFINED-ON-LINE '298) 
(PUT 'TAYSIMPMAINVAR 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'TAYSIMPMAINVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPMAINVAR (U)
    (COND ((NOT (SFP U)) (TAYSIMPKERNEL U)) (T (CONS (TAYSIMPF U) 1)))) 
(PUT 'TAYSIMPKERNEL 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPKERNEL 'DEFINED-ON-LINE '303) 
(PUT 'TAYSIMPKERNEL 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'TAYSIMPKERNEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPKERNEL (U)
    (PROG (FN X)
      (SETQ U (SIMP* U))
      (COND ((NOT (KERNP U)) (RETURN U))
            (T
             (PROGN
              (SETQ X (CAAAR (CAR U)))
              (COND ((OR (ATOM X) (EQCAR X 'TAYLOR*)) (RETURN U)))
              (SETQ FN (GET (CAR X) 'TAYLORSIMPFN))
              (RETURN (COND ((NULL FN) U) (T (APPLY1 FN X))))))))) 
(PUT 'EXPTTAYI 'NUMBER-OF-ARGS 2) 
(PUT 'EXPTTAYI 'DEFINED-ON-LINE '315) 
(PUT 'EXPTTAYI 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'EXPTTAYI 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPTTAYI (U I)
    (PROG (V FLG)
      (COND ((LESSP I 0) (PROGN (SETQ I (MINUS I)) (SETQ FLG T))))
      (SETQ V
              (COND
               ((EVENP I)
                ((LAMBDA (G156 G157)
                   (LIST 'TAYLOR*
                         (LIST
                          (CONS
                           (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                             (SETQ EL G157)
                             (COND ((NULL EL) (RETURN NIL)))
                             (SETQ FORALL-RESULT
                                     (SETQ FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (EL)
                                                 (NLIST 0 (LENGTH (CAR EL))))
                                               (CAR EL))
                                              NIL)))
                            LOOPLABEL
                             (SETQ EL (CDR EL))
                             (COND ((NULL EL) (RETURN FORALL-RESULT)))
                             (RPLACD FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (EL)
                                         (NLIST 0 (LENGTH (CAR EL))))
                                       (CAR EL))
                                      NIL))
                             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                             (GO LOOPLABEL))
                           G156))
                         G157 G156 NIL))
                 (CONS 1 1) (CADDR U)))
               (T (PROGN (SETQ I (DIFFERENCE I 1)) U))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP (SETQ I (QUOTIENT I 2)) 0)) (RETURN NIL)))
        (PROGN
         (SETQ U (MULTTAYLOR U U))
         (COND ((NOT (EVENP I)) (SETQ V (MULTTAYLOR V U)))))
        (GO WHILELABEL))
      (RETURN (COND (FLG (INVTAYLOR V)) (T V))))) 
(PUT 'EXPTTAYRAT 'NUMBER-OF-ARGS 2) 
(PUT 'EXPTTAYRAT 'DEFINED-ON-LINE '385) 
(PUT 'EXPTTAYRAT 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'EXPTTAYRAT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXPTTAYRAT (TAY RAT)
    (PROG (CLIST TC TP)
      (COND
       ((NOT (EQCAR TAY 'TAYLOR*))
        (RETURN (SIMP* (LIST 'EXPT TAY (MK*SQ RAT))))))
      (SETQ TC
              ((LAMBDA (CFLIS)
                 (PROGN
                  (PROG ()
                   WHILELABEL
                    (COND
                     ((NOT
                       (AND (NOT (NULL CFLIS)) (NULL (CAR (CDR (CAR CFLIS))))))
                      (RETURN NIL)))
                    (SETQ CFLIS (CDR CFLIS))
                    (GO WHILELABEL))
                  CFLIS))
               (CADR TAY)))
      (SETQ TP (CADDR TAY))
      (COND
       ((NULL TC)
        (COND
         ((TAYEXP-MINUSP (CAR RAT)) (TAYLOR-ERROR* 'NOT-A-UNIT 'EXPTTAYRAT))
         (T
          (PROGN
           (SETQ TP
                   (PROG (TPEL FORALL-RESULT FORALL-ENDPTR)
                     (SETQ TPEL TP)
                     (COND ((NULL TPEL) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (TPEL)
                                         (PROG (W)
                                           (SETQ W
                                                   (TAYEXP-TIMES (CADDDR TPEL)
                                                                 (*Q2TAYEXP
                                                                  RAT)))
                                           (RETURN
                                            (LIST (CAR TPEL) (CADR TPEL)
                                                  (TAYEXP-DIFFERENCE W
                                                                     (MKRN 1
                                                                           (CDR
                                                                            RAT)))
                                                  W))))
                                       (CAR TPEL))
                                      NIL)))
                    LOOPLABEL
                     (SETQ TPEL (CDR TPEL))
                     (COND ((NULL TPEL) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (TPEL)
                                 (PROG (W)
                                   (SETQ W
                                           (TAYEXP-TIMES (CADDDR TPEL)
                                                         (*Q2TAYEXP RAT)))
                                   (RETURN
                                    (LIST (CAR TPEL) (CADR TPEL)
                                          (TAYEXP-DIFFERENCE W
                                                             (MKRN 1
                                                                   (CDR RAT)))
                                          W))))
                               (CAR TPEL))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
           (SETQ CLIST
                   (LIST
                    (CONS
                     (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                       (SETQ EL TP)
                       (COND ((NULL EL) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (NLIST 0 (LENGTH (CAR EL))))
                                         (CAR EL))
                                        NIL)))
                      LOOPLABEL
                       (SETQ EL (CDR EL))
                       (COND ((NULL EL) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                 (CAR EL))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     (CONS NIL 1))))))))
       (T
        (PROG (C0 L L1)
          (SETQ C0 (CAR TC))
          (SETQ L1
                  (PROG (LL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ LL (CAR C0))
                    (COND ((NULL LL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (LL)
                                        (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ P LL)
                                          (COND ((NULL P) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (P)
                                                              (TAYEXP-TIMES P
                                                                            (*Q2TAYEXP
                                                                             RAT)))
                                                            (CAR P))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ P (CDR P))
                                          (COND
                                           ((NULL P) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (P)
                                                      (TAYEXP-TIMES P
                                                                    (*Q2TAYEXP
                                                                     RAT)))
                                                    (CAR P))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL)))
                                      (CAR LL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ LL (CDR LL))
                    (COND ((NULL LL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (LL)
                                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ P LL)
                                  (COND ((NULL P) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (P)
                                                      (TAYEXP-TIMES P
                                                                    (*Q2TAYEXP
                                                                     RAT)))
                                                    (CAR P))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ P (CDR P))
                                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (P)
                                              (TAYEXP-TIMES P (*Q2TAYEXP RAT)))
                                            (CAR P))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL)))
                              (CAR LL))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ L
                  (PROG (NL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ NL (CAR C0))
                    (COND ((NULL NL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (NL)
                                        (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ P NL)
                                          (COND ((NULL P) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (P)
                                                              (TAYEXP-MINUS P))
                                                            (CAR P))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ P (CDR P))
                                          (COND
                                           ((NULL P) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (P)
                                                      (TAYEXP-MINUS P))
                                                    (CAR P))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL)))
                                      (CAR NL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ NL (CDR NL))
                    (COND ((NULL NL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (NL)
                                (PROG (P FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ P NL)
                                  (COND ((NULL P) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (P)
                                                      (TAYEXP-MINUS P))
                                                    (CAR P))
                                                   NIL)))
                                 LOOPLABEL
                                  (SETQ P (CDR P))
                                  (COND ((NULL P) (RETURN FORALL-RESULT)))
                                  (RPLACD FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (P) (TAYEXP-MINUS P))
                                            (CAR P))
                                           NIL))
                                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                  (GO LOOPLABEL)))
                              (CAR NL))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ TP
                  (ADDTO-ALL-TAYTPELORDERS TP
                   (PROG (DG FORALL-RESULT FORALL-ENDPTR)
                     (SETQ DG L)
                     (COND ((NULL DG) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (DG)
                                         (PROG (N FORALL-RESULT)
                                           (SETQ N DG)
                                           (SETQ FORALL-RESULT 0)
                                          LAB1
                                           (COND
                                            ((NULL N) (RETURN FORALL-RESULT)))
                                           (SETQ FORALL-RESULT
                                                   (TAYEXP-PLUS
                                                    ((LAMBDA (N) N) (CAR N))
                                                    FORALL-RESULT))
                                           (SETQ N (CDR N))
                                           (GO LAB1)))
                                       (CAR DG))
                                      NIL)))
                    LOOPLABEL
                     (SETQ DG (CDR DG))
                     (COND ((NULL DG) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (DG)
                                 (PROG (N FORALL-RESULT)
                                   (SETQ N DG)
                                   (SETQ FORALL-RESULT 0)
                                  LAB1
                                   (COND ((NULL N) (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (TAYEXP-PLUS
                                            ((LAMBDA (N) N) (CAR N))
                                            FORALL-RESULT))
                                   (SETQ N (CDR N))
                                   (GO LAB1)))
                               (CAR DG))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))
          (SETQ L (CONS L (INVSQ (CDR C0))))
          (SETQ L
                  (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ EL (CDR TC))
                    (COND ((NULL EL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL)
                                        (CONS (ADD-DEGREES (CAR EL) (CAR L))
                                              (MULTSQ (CDR EL) (CDR L))))
                                      (CAR EL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ EL (CDR EL))
                    (COND ((NULL EL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (EL)
                                (CONS (ADD-DEGREES (CAR EL) (CAR L))
                                      (MULTSQ (CDR EL) (CDR L))))
                              (CAR EL))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ CLIST (EXPTTAYRAT1 TP L RAT))
          (SETQ C0
                  (CONS L1
                        (SIMP*
                         (LIST 'EXPT (MK*SQ (CDR C0))
                               (LIST 'QUOTIENT (CAR RAT) (CDR RAT))))))
          (SETQ CLIST
                  (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ EL CLIST)
                    (COND ((NULL EL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL)
                                        (CONS (ADD-DEGREES (CAR EL) (CAR C0))
                                              (MULTSQ (CDR EL) (CDR C0))))
                                      (CAR EL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ EL (CDR EL))
                    (COND ((NULL EL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (EL)
                                (CONS (ADD-DEGREES (CAR EL) (CAR C0))
                                      (MULTSQ (CDR EL) (CDR C0))))
                              (CAR EL))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
          (SETQ TP
                  (ADDTO-ALL-TAYTPELORDERS TP
                   (PROG (DG FORALL-RESULT FORALL-ENDPTR)
                     (SETQ DG L1)
                     (COND ((NULL DG) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (DG)
                                         (PROG (N FORALL-RESULT)
                                           (SETQ N DG)
                                           (SETQ FORALL-RESULT 0)
                                          LAB1
                                           (COND
                                            ((NULL N) (RETURN FORALL-RESULT)))
                                           (SETQ FORALL-RESULT
                                                   (TAYEXP-PLUS
                                                    ((LAMBDA (N) N) (CAR N))
                                                    FORALL-RESULT))
                                           (SETQ N (CDR N))
                                           (GO LAB1)))
                                       (CAR DG))
                                      NIL)))
                    LOOPLABEL
                     (SETQ DG (CDR DG))
                     (COND ((NULL DG) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (DG)
                                 (PROG (N FORALL-RESULT)
                                   (SETQ N DG)
                                   (SETQ FORALL-RESULT 0)
                                  LAB1
                                   (COND ((NULL N) (RETURN FORALL-RESULT)))
                                   (SETQ FORALL-RESULT
                                           (TAYEXP-PLUS
                                            ((LAMBDA (N) N) (CAR N))
                                            FORALL-RESULT))
                                   (SETQ N (CDR N))
                                   (GO LAB1)))
                               (CAR DG))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))))))
      (RETURN
       (LIST 'TAYLOR* CLIST TP
             (COND
              ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
               (SIMP
                (LIST 'EXPT (PREPSQ (CADDDR TAY))
                      (LIST 'QUOTIENT (CAR RAT) (CDR RAT)))))
              (T NIL))
             (CAR (CDDDDR TAY)))))) 
(PUT 'EXPTTAYRAT1 'NUMBER-OF-ARGS 3) 
(PUT 'EXPTTAYRAT1 'DEFINED-ON-LINE '453) 
(PUT 'EXPTTAYRAT1 'DEFINED-IN-FILE 'TAYLOR/TAYSIMP.RED) 
(PUT 'EXPTTAYRAT1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE EXPTTAYRAT1 (TP TCL RAT)
    (PROG (CLIST COEFFLIS IL L0 RAT1)
      (SETQ RAT1 (ADDSQ RAT (CONS 1 1)))
      (SETQ L0
              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                (SETQ EL TP)
                (COND ((NULL EL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL))))
                                  (CAR EL))
                                 NIL)))
               LOOPLABEL
                (SETQ EL (CDR EL))
                (COND ((NULL EL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (EL) (NLIST 0 (LENGTH (CAR EL)))) (CAR EL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CLIST (LIST (CONS L0 (CONS 1 1))))
      (SETQ TCL (CONS (CONS L0 (CONS 1 1)) TCL))
      (SETQ IL (SMALLEST-INCREMENT TCL))
      (SETQ COEFFLIS
              (MAKECOEFFS0 TP
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X TP)
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (CADDDR X)) (CAR X)) NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (CADDDR X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               IL))
      (COND ((NULL COEFFLIS) (RETURN CLIST)))
      (PROG (CC)
        (SETQ CC (CDR COEFFLIS))
       LAB
        (COND ((NULL CC) (RETURN NIL)))
        ((LAMBDA (CC)
           (PROG (S POS PP Q N N1)
             (SETQ S (CONS NIL 1))
             (SETQ POS (FIND-NON-ZERO CC))
             (SETQ N (NTH (NTH CC (CAR POS)) (CDR POS)))
             (SETQ PP (MAKECOEFFPAIRS L0 CC L0 IL))
             (PROG (P)
               (SETQ P PP)
              LAB
               (COND ((NULL P) (RETURN NIL)))
               ((LAMBDA (P)
                  (PROG (V W)
                    (SETQ V
                            ((LAMBDA (CC)
                               (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                             (ASSOC (CDR P) TCL)))
                    (SETQ W
                            ((LAMBDA (CC)
                               (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                             (ASSOC (CAR P) CLIST)))
                    (COND ((OR (NULL (CAR V)) (NULL (CAR W))) (RETURN NIL)))
                    (SETQ W (MULTSQ W V))
                    (SETQ N1 (NTH (NTH (CAR P) (CAR POS)) (CDR POS)))
                    (SETQ Q
                            (MULTSQ (*TAYEXP2Q (TAYEXP-MINUS N1))
                                    (INVSQ (*TAYEXP2Q N))))
                    (SETQ S (ADDSQ S (MULTSQ (ADDSQ RAT (MULTSQ Q RAT1)) W)))))
                (CAR P))
               (SETQ P (CDR P))
               (GO LAB))
             (COND
              ((NOT (NULL (CAR S))) (SETQ CLIST (CONS (CONS CC S) CLIST))))))
         (CAR CC))
        (SETQ CC (CDR CC))
        (GO LAB))
      (RETURN (REVERSIP CLIST)))) 
(ENDMODULE) 