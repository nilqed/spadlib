(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYFNS)) 
(EXPORTS
 (LIST 'TAYSIMPEXPT 'TAYSIMPATAN 'TAYSIMPLOG 'TAYSIMPEXP 'TAYSIMPTAN
       'TAYSIMPSIN 'TAYSIMPSINH 'TAYSIMPASIN)) 
(IMPORTS
 (LIST '*F2Q '|:MINUSP| 'ADDSQ 'AEVAL 'DENR 'DOMAINP 'EQCAR 'EVENP 'FREEOF
       'INVSQ 'KERNP 'LASTPAIR 'LET 'LPRIM 'LNC 'MK*SQ 'MKSQ 'MULTSQ 'MVAR
       'NEGSQ 'NEQ 'NLIST 'NTH 'NUMR 'OVER 'PREPD 'PREPSQ 'QUOTSQ 'RETIMES
       'REVAL 'REVERSIP 'SIMP 'SIMP* 'SIMPLOGI 'SIMPLOGSQ 'SUBS2* 'SUBSQ
       'SUBTRSQ '*TAY2Q '*TAYEXP2Q 'CONSTANT-SQ-P 'CST-TAYLOR* 'FIND-NON-ZERO
       'GET-DEGREE 'HAS-TAYVARS 'MAKE-CST-COEFFLIS 'MAKE-CST-POWERLIST
       'MAKE-TAYLOR* 'PRUNE-COEFFLIST 'SET-TAYCOEFFLIST 'SET-TAYCFSQ
       'SET-TAYFLAGS 'SET-TAYORIG 'TAYCFPL 'TAYCFSQ 'TAYCOEFFLIST 'TAYFLAGS
       'TAYGETCOEFF 'TAYLOR*P 'TAYLOR-KERNEL-SQ-P '|TAYLOR:| 'TAYMAKECOEFF
       'TAYORIG 'TAYTEMPLATE 'TAYTPELNEXT 'TAYTPELORDER 'TAYTPELPOINT
       'TAYTPELVARS 'TAYTPVARS 'TAYVARS 'TPNEXTLIST 'CONFUSION 'DELETE-NTH
       'DELETE-NTH-NTH 'REPLACE-NTH 'REPLACE-NTH-NTH 'TAYLOR-ERROR
       'TAYLOR-ERROR* 'VAR-IS-NTH 'ADDTO-ALL-TAYTPELORDERS 'GET-CST-COEFF
       'IS-NEG-PL 'SMALLEST-INCREMENT 'SUBTR-DEGREES 'TAYLOR*-CONSTANTP
       'TAYLOR*-ZEROP 'ADDTAYLOR 'ADDTAYLOR-AS-SQ 'INVTAYLOR 'MAKECOEFFS0
       'MAKECOEFFPAIRS 'MAKECOEFFPAIRS1 'MULTTAYLOR 'MULTTAYLOR-AS-SQ
       'MULTTAYLORSQ 'NEGTAYLOR 'NEGTAYLOR1 'QUOTTAYLOR 'TAYLOREXPAND
       'EXPTTAYRAT 'TAYSIMPSQ 'TAYSIMPSQ* 'DIFFTAYLORWRTTAYVAR 'PREPTAYCOEFF
       'PREPTAYLOR* 'TAYLORCOMBINE 'TAYLORTOSTANDARD)) 
(FLUID '(*TAYLORKEEPORIGINAL **TAYLOR-EPSILON** FRLIS*)) 
(PUT 'TAYSIMPEXPT 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPEXPT 'DEFINED-ON-LINE '92) 
(PUT 'TAYSIMPEXPT 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPEXPT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPEXPT (U)
    (COND ((OR (NOT (EQ (CAR U) 'EXPT)) (CDDDR U)) (CONFUSION 'TAYSIMPEXPT))
          ((EQ (CADR U) 'E) (TAYSIMPEXP (LIST 'EXP (CADDR U))))
          (T
           (PROG (BAS EXPN)
             (SETQ BAS (TAYSIMPSQ (SIMP* (CADR U))))
             (SETQ EXPN (TAYSIMPSQ (SIMP* (CADDR U))))
             (COND
              ((CONSTANT-SQ-P BAS)
               (RETURN
                (TAYSIMPEXP
                 (LIST 'EXP
                       (MK*SQ
                        (MULTSQ (SIMP* (LIST 'LOG (MK*SQ BAS))) EXPN)))))))
             (COND
              ((NULL (KERNP BAS))
               (COND
                ((NOT (EQUAL (CDR BAS) 1))
                 (RETURN (MKSQ (LIST 'EXPT (PREPSQ BAS) (PREPSQ EXPN)) 1)))
                ((OR (ATOM (CAR BAS)) (ATOM (CAR (CAR BAS))))
                 (RETURN
                  (TAYSIMPEXP
                   (LIST 'EXP
                         (PREPSQ
                          (MULTSQ (SIMP* (LIST 'LOG (PREPD (CAR BAS))))
                                  EXPN))))))
                (T
                 (RETURN (MKSQ (LIST 'EXPT (PREPSQ BAS) (PREPSQ EXPN)) 1))))))
             (COND
              ((AND (FIXP (CAR EXPN)) (FIXP (CDR EXPN)))
               (RETURN
                (CONS
                 (LIST
                  (CONS
                   (GETPOWER (FKERN (EXPTTAYRAT (CAAAR (CAR BAS)) EXPN)) 1) 1))
                 1))))
             (COND
              ((AND (EQUAL (CDR EXPN) 1) (EQCAR (CAR EXPN) '|:RN:|))
               (RETURN
                (CONS
                 (LIST
                  (CONS
                   (GETPOWER
                    (FKERN (EXPTTAYRAT (CAAAR (CAR BAS)) (CDR (CAR EXPN)))) 1)
                   1))
                 1))))
             (SETQ BAS (CAAAR (CAR BAS)))
             (RETURN
              (COND
               ((EQCAR BAS 'TAYLOR*)
                (COND
                 ((AND (KERNP EXPN) (EQCAR (CAAAR (CAR EXPN)) 'TAYLOR*))
                  (COND
                   ((EQUAL (CADDR BAS) (CADDR (CAAAR (CAR EXPN))))
                    (TAYSIMPEXP
                     (LIST 'EXP
                           (MK*SQ
                            (TAYSIMPSQ
                             (MULTTAYLOR-AS-SQ EXPN
                              (TAYSIMPLOG (LIST 'LOG BAS))))))))
                   (T (MKSQ (LIST 'EXPT BAS (CAAAR (CAR EXPN))) 1))))
                 ((NOT
                   (SMEMBERLP
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (CADDR BAS))
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
                    EXPN))
                  (PROG (LOGTERM)
                    (SETQ LOGTERM (TAYSIMPLOG (LIST 'LOG BAS)))
                    (RETURN
                     (COND
                      ((AND (KERNP LOGTERM)
                            (EQCAR (CAAAR (CAR LOGTERM)) 'TAYLOR*))
                       (TAYSIMPEXP
                        (LIST 'EXP (MULTTAYLORSQ (CAAAR (CAR LOGTERM)) EXPN))))
                      (T
                       (TAYSIMPSQ
                        (SIMP* (LIST 'EXP (MK*SQ (MULTSQ LOGTERM EXPN))))))))))
                 (T (MKSQ (LIST 'EXPT BAS (MK*SQ EXPN)) 1))))
               ((AND (KERNP EXPN) (EQCAR (CAAAR (CAR EXPN)) 'TAYLOR*))
                (COND
                 ((NOT
                   (SMEMBERLP
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (CADDR (CAAAR (CAR EXPN))))
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
                    BAS))
                  (TAYSIMPEXP
                   (LIST 'EXP
                         (MULTTAYLORSQ (CAAAR (CAR EXPN))
                          (SIMP* (LIST 'LOG BAS))))))
                 ((TAYLOR*-CONSTANTP (CAAAR (CAR EXPN)))
                  (TAYLOREXPAND
                   (SIMP* (LIST 'EXPT BAS (PREPTAYLOR* (CAAAR (CAR EXPN)))))
                   (CADDR (CAAAR (CAR EXPN)))))
                 (T (MKSQ (LIST 'EXPT BAS (MK*SQ EXPN)) 1))))
               (T (MKSQ (LIST 'EXPT BAS (MK*SQ EXPN)) 1)))))))) 
(PUT 'EXPT 'TAYLORSIMPFN 'TAYSIMPEXPT) 
(PUT 'TAYCOEFFLIST-UNION 'NUMBER-OF-ARGS 1) 
(PUT 'TAYCOEFFLIST-UNION 'DEFINED-ON-LINE '157) 
(PUT 'TAYCOEFFLIST-UNION 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYCOEFFLIST-UNION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYCOEFFLIST-UNION (U)
    (COND ((NULL (CDR U)) (CAR U))
          (T (TAYCOEFFLIST-UNION2 (CAR U) (TAYCOEFFLIST-UNION (CDR U)))))) 
(PUT 'TAYCOEFFLIST-UNION2 'NUMBER-OF-ARGS 2) 
(PUT 'TAYCOEFFLIST-UNION2 'DEFINED-ON-LINE '161) 
(PUT 'TAYCOEFFLIST-UNION2 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYCOEFFLIST-UNION2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYCOEFFLIST-UNION2 (X Y)
    (PROGN
     (PROG (W)
       (SETQ W Y)
      LAB
       (COND ((NULL W) (RETURN NIL)))
       ((LAMBDA (W) (COND ((NULL (ASSOC (CAR W) X)) (SETQ X (CONS W X)))))
        (CAR W))
       (SETQ W (CDR W))
       (GO LAB))
     X)) 
(PUT 'INTTAYLORWRTTAYVAR 'NUMBER-OF-ARGS 2) 
(PUT 'INTTAYLORWRTTAYVAR 'DEFINED-ON-LINE '169) 
(PUT 'INTTAYLORWRTTAYVAR 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'INTTAYLORWRTTAYVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INTTAYLORWRTTAYVAR (TAY VAR)
    (INTTAYLORWRTTAYVAR1 (CADR TAY) (CADDR TAY) VAR)) 
(PUT 'INTTAYLORWRTTAYVAR1 'NUMBER-OF-ARGS 3) 
(PUT 'INTTAYLORWRTTAYVAR1 'DEFINED-ON-LINE '175) 
(PUT 'INTTAYLORWRTTAYVAR1 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'INTTAYLORWRTTAYVAR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE INTTAYLORWRTTAYVAR1 (TCL TP VAR)
    (PROG (TT U W SINGLIST CSING N N1 M)
      (SETQ N 0)
      (SETQ N1 0)
      (SETQ M 0)
      (SETQ U (VAR-IS-NTH TP VAR))
      (SETQ N (CAR U))
      (SETQ N1 (CDR U))
      (SETQ TT (NTH TP N))
      (SETQ U
              (PROG (CC FORALL-RESULT FORALL-ENDPTR)
                (SETQ CC TCL)
               STARTOVER
                (COND ((NULL CC) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (CC)
                           (PROGN
                            (SETQ M (NTH (NTH (CAR CC) N) N1))
                            (COND
                             ((EQ (CADR (NTH TP N)) 'INFINITY)
                              (PROGN
                               (COND
                                ((EQUAL M 1)
                                 (PROGN
                                  (SETQ SINGLIST
                                          (CONS
                                           (CONS (DELETE-NTH-NTH (CAR CC) N N1)
                                                 (CDR CC))
                                           SINGLIST))
                                  NIL))
                                (T
                                 (LIST
                                  (CONS
                                   (REPLACE-NTH-NTH (CAR CC) N N1
                                    (TAYEXP-DIFFERENCE M 1))
                                   (MULTSQ (CDR CC)
                                           (INVSQ
                                            (*TAYEXP2Q
                                             (TAYEXP-PLUS (TAYEXP-MINUS M)
                                                          1))))))))))
                             (T
                              (PROGN
                               (COND
                                ((EQUAL M (TAYEXP-MINUS 1))
                                 (PROGN
                                  (SETQ SINGLIST
                                          (CONS
                                           (CONS (DELETE-NTH-NTH (CAR CC) N N1)
                                                 (CDR CC))
                                           SINGLIST))
                                  NIL))
                                (T
                                 (LIST
                                  (CONS
                                   (REPLACE-NTH-NTH (CAR CC) N N1
                                    (TAYEXP-PLUS M 1))
                                   (MULTSQ (CDR CC)
                                           (INVSQ
                                            (*TAYEXP2Q
                                             (TAYEXP-PLUS M 1)))))))))))))
                         (CAR CC)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ CC (CDR CC))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL CC) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (CC)
                           (PROGN
                            (SETQ M (NTH (NTH (CAR CC) N) N1))
                            (COND
                             ((EQ (CADR (NTH TP N)) 'INFINITY)
                              (PROGN
                               (COND
                                ((EQUAL M 1)
                                 (PROGN
                                  (SETQ SINGLIST
                                          (CONS
                                           (CONS (DELETE-NTH-NTH (CAR CC) N N1)
                                                 (CDR CC))
                                           SINGLIST))
                                  NIL))
                                (T
                                 (LIST
                                  (CONS
                                   (REPLACE-NTH-NTH (CAR CC) N N1
                                    (TAYEXP-DIFFERENCE M 1))
                                   (MULTSQ (CDR CC)
                                           (INVSQ
                                            (*TAYEXP2Q
                                             (TAYEXP-PLUS (TAYEXP-MINUS M)
                                                          1))))))))))
                             (T
                              (PROGN
                               (COND
                                ((EQUAL M (TAYEXP-MINUS 1))
                                 (PROGN
                                  (SETQ SINGLIST
                                          (CONS
                                           (CONS (DELETE-NTH-NTH (CAR CC) N N1)
                                                 (CDR CC))
                                           SINGLIST))
                                  NIL))
                                (T
                                 (LIST
                                  (CONS
                                   (REPLACE-NTH-NTH (CAR CC) N N1
                                    (TAYEXP-PLUS M 1))
                                   (MULTSQ (CDR CC)
                                           (INVSQ
                                            (*TAYEXP2Q
                                             (TAYEXP-PLUS M 1)))))))))))))
                         (CAR CC)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ CC (CDR CC))
                (GO LOOPLABEL)))
      (SETQ W
              (LIST (CAR TT) (CADR TT)
                    (COND
                     ((MEMBER VAR (CAR TT))
                      (COND
                       ((EQ (CADR TT) 'INFINITY)
                        (TAYEXP-DIFFERENCE (CADDR TT) 1))
                       (T (TAYEXP-PLUS (CADDR TT) 1))))
                     (T (CADDR TT)))
                    (COND
                     ((MEMBER VAR (CAR TT))
                      (COND
                       ((EQ (CADR TT) 'INFINITY)
                        (TAYEXP-DIFFERENCE (CADDDR TT) 1))
                       (T (TAYEXP-PLUS (CADDDR TT) 1))))
                     (T (CADDR TT)))))
      (COND
       (SINGLIST
        (PROG (TPEL)
          (SETQ TPEL (NTH TP N))
          (SETQ SINGLIST (REVERSIP SINGLIST))
          (COND
           ((EQUAL (CAR (CAR SINGLIST)) '(NIL))
            (SETQ CSING (CDR (CAR SINGLIST))))
           (T
            (SETQ CSING
                    (CONS
                     (LIST
                      (CONS
                       (GETPOWER
                        (FKERN
                         (LIST 'TAYLOR* SINGLIST
                               (REPLACE-NTH TP N
                                (LIST (DELETE-NTH (CAR TPEL) N1) (CADR TPEL)
                                      (CADDR TPEL) (CADDDR TPEL)))
                               NIL NIL))
                        1)
                       1))
                     1))))
          (SETQ CSING
                  (MULTSQ CSING (SIMP* (LIST 'LOG (NTH (CAR TPEL) N1))))))))
      (RETURN (CONS CSING (LIST 'TAYLOR* U (REPLACE-NTH TP N W) NIL NIL))))) 
(PUT 'TAYSIMPASIN 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPASIN 'DEFINED-ON-LINE '391) 
(PUT 'TAYSIMPASIN 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPASIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPASIN (U)
    (COND
     ((OR (NOT (MEMQ (CAR U) '(ASIN ACOS ACSC ASEC ASINH ACOSH ACSCH ASECH)))
          (CDDR U))
      (CONFUSION 'TAYSIMPASIN))
     (T
      (PROG (L L0 C0 V TAY0 TAY TAY2 TP SINGLIST)
        (SETQ TAY0 (TAYSIMPSQ (SIMP* (CADR U))))
        (COND
         ((NOT (AND (KERNP TAY0) (EQCAR (CAAAR (CAR TAY0)) 'TAYLOR*)))
          (RETURN (MKSQ (LIST (CAR U) (MK*SQ TAY0)) 1))))
        (SETQ TAY0 (CAAAR (CAR TAY0)))
        (SETQ L0
                (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                  (SETQ EL (CADDR TAY0))
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
        (SETQ C0
                ((LAMBDA (CC) (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                 (ASSOC L0 (CADR TAY0))))
        (COND
         ((MEMQ (CAR U) '(ASEC ACSC ASECH ACSCH))
          (COND ((NULL (CAR C0)) (RETURN (TAYSIMPASEC* (CAR U) TAY0)))
                (T (SETQ TAY (INVTAYLOR TAY0)))))
         (T (SETQ TAY TAY0)))
        (SETQ TP (CADDR TAY))
        (SETQ L
                ((LAMBDA (CFLIS)
                   (PROGN
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND (NOT (NULL CFLIS))
                              (NULL (CAR (CDR (CAR CFLIS))))))
                        (RETURN NIL)))
                      (SETQ CFLIS (CDR CFLIS))
                      (GO WHILELABEL))
                    CFLIS))
                 (CADR TAY)))
        (COND
         ((NULL L)
          (RETURN
           (CONS
            (LIST
             (CONS
              (GETPOWER
               (FKERN
                (LIST 'TAYLOR*
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
                        (SIMP* (LIST (CAR U) 0))))
                      TP
                      (COND ((NULL (CADDDR TAY)) NIL)
                            (T (SIMP* (LIST (CAR U) (PREPSQ (CADDDR TAY))))))
                      NIL))
               1)
              1))
            1))))
        (COND ((IS-NEG-PL (CAR (CAR L))) (RETURN (TAYSIMPASIN* (CAR U) TAY))))
        (SETQ TAY2 (MULTTAYLOR TAY TAY))
        (COND
         ((MEMQ (CAR U) '(ASIN ACOS ACSC ASEC)) (SETQ TAY2 (NEGTAYLOR TAY2))))
        (SETQ TAY2
                (ADDTAYLOR
                 ((LAMBDA (G169)
                    (LIST 'TAYLOR*
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
                                       ((LAMBDA (EL)
                                          (NLIST 0 (LENGTH (CAR EL))))
                                        (CAR EL))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL))
                            G169))
                          TP G169 NIL))
                  (CONS
                   (COND ((MEMQ (CAR U) '(ACOSH ASECH)) (TAYEXP-MINUS 1))
                         (T 1))
                   1))
                 TAY2))
        (COND ((TAYLOR*-ZEROP TAY2) (TAYLOR-ERROR* 'BRANCH-POINT (CAR U)))
              ((NULL
                (CAR
                 ((LAMBDA (CC) (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                  (ASSOC L0 (CADR TAY2)))))
               (RETURN (TAYSIMPASIN* (CAR U) TAY))))
        (SETQ TAY2 (INVTAYLOR (EXPTTAYRAT TAY2 (CONS 1 2))))
        (COND ((MEMQ (CAR U) '(ACOS ASEC)) (SETQ TAY2 (NEGTAYLOR TAY2))))
        (SETQ L
                (PROG (KRNL FORALL-RESULT FORALL-ENDPTR)
                  (SETQ KRNL
                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                            (SETQ X (CADDR TAY))
                           STARTOVER
                            (COND ((NULL X) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (X) (APPEND (CAR X) NIL))
                                     (CAR X)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ X (CDR X))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL X) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (X) (APPEND (CAR X) NIL))
                                     (CAR X)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ X (CDR X))
                            (GO LOOPLABEL)))
                  (COND ((NULL KRNL) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (KRNL)
                                      (INTTAYLORWRTTAYVAR
                                       (MULTTAYLOR
                                        (DIFFTAYLORWRTTAYVAR TAY KRNL) TAY2)
                                       KRNL))
                                    (CAR KRNL))
                                   NIL)))
                 LOOPLABEL
                  (SETQ KRNL (CDR KRNL))
                  (COND ((NULL KRNL) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (KRNL)
                              (INTTAYLORWRTTAYVAR
                               (MULTTAYLOR (DIFFTAYLORWRTTAYVAR TAY KRNL) TAY2)
                               KRNL))
                            (CAR KRNL))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ V
                (TAYCOEFFLIST-UNION
                 (PROG (PP FORALL-RESULT FORALL-ENDPTR)
                   (SETQ PP L)
                   (COND ((NULL PP) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (PP) (CADR (CDR PP))) (CAR PP))
                                    NIL)))
                  LOOPLABEL
                   (SETQ PP (CDR PP))
                   (COND ((NULL PP) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (PP) (CADR (CDR PP))) (CAR PP)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
        (SETQ SINGLIST (CONS NIL 1))
        (PROG (PP)
          (SETQ PP L)
         LAB
          (COND ((NULL PP) (RETURN NIL)))
          ((LAMBDA (PP)
             (COND ((CAR PP) (SETQ SINGLIST (ADDSQ SINGLIST (CAR PP))))))
           (CAR PP))
          (SETQ PP (CDR PP))
          (GO LAB))
        (SETQ C0 (SIMP (LIST (CAR U) (MK*SQ C0))))
        (SETQ V (CONS (CONS L0 C0) V))
        (SETQ TAY
                (LIST 'TAYLOR* V TP
                      (COND
                       ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
                        (SIMP (LIST (CAR U) (MK*SQ (CADDDR TAY)))))
                       (T NIL))
                      (CAR (CDDDDR TAY))))
        (COND
         ((NULL (CAR SINGLIST))
          (RETURN (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1))))
        (COND
         ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
          (RPLACA (CDDDR TAY) (ADDSQ (CADDDR TAY) (NEGSQ SINGLIST)))))
        (RETURN
         (ADDSQ SINGLIST (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1))))))) 
(PUT 'TAYSIMPASEC* 'NUMBER-OF-ARGS 2) 
(PUT 'TAYSIMPASEC* 'DEFINED-ON-LINE '457) 
(PUT 'TAYSIMPASEC* 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPASEC* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYSIMPASEC* (FN TAY)
    (PROG (RESULT TAY1 TAY2 I1)
      (SETQ I1 (SIMP 'I))
      (COND ((MEMQ FN '(ASIN ACSC)) (SETQ TAY (MULTTAYLORSQ TAY I1))))
      (SETQ TAY1
              ((LAMBDA (G171 G172)
                 (LIST 'TAYLOR*
                       (LIST
                        (CONS
                         (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                           (SETQ EL G172)
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
                         G171))
                       G172 G171 NIL))
               (CONS 1 1) (CADDR TAY)))
      (SETQ TAY2 (MULTTAYLOR TAY TAY))
      (COND ((MEMQ FN '(ASEC ASECH)) (SETQ TAY2 (NEGTAYLOR TAY2))))
      (SETQ RESULT
              (TAYSIMPLOG
               (LIST 'LOG
                     (ADDTAYLOR (EXPTTAYRAT (ADDTAYLOR TAY2 TAY1) (CONS 1 2))
                      TAY1))))
      (SETQ TAY1 (TAYSIMPLOG (LIST 'LOG TAY)))
      (COND
       ((MEMQ FN '(ASIN ACOS ASEC ACSC))
        (PROGN
         (SETQ RESULT (TAYSIMPSQ (NEGSQ (MULTSQ RESULT I1))))
         (SETQ RESULT (ADDTAYLOR-AS-SQ RESULT (MULTSQ I1 TAY1)))))
       (T
        (SETQ RESULT
                (ADDTAYLOR-AS-SQ RESULT
                 (NEGSQ (TAYSIMPLOG (LIST 'LOG TAY)))))))
      (RETURN (TAYSIMPSQ* RESULT)))) 
(PUT 'TAYSIMPASIN* 'NUMBER-OF-ARGS 2) 
(PUT 'TAYSIMPASIN* 'DEFINED-ON-LINE '477) 
(PUT 'TAYSIMPASIN* 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPASIN* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYSIMPASIN* (FN TAY)
    (PROG (RESULT TAY1)
      (COND ((MEMQ FN '(ASIN ACSC)) (SETQ TAY (MULTTAYLORSQ TAY (SIMP 'I)))))
      (SETQ TAY1
              ((LAMBDA (G173 G174)
                 (LIST 'TAYLOR*
                       (LIST
                        (CONS
                         (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                           (SETQ EL G174)
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
                         G173))
                       G174 G173 NIL))
               (CONS
                (COND ((MEMQ FN '(ASIN ASINH ACSC ACSCH)) 1) (T (MINUS 1))) 1)
               (CADDR TAY)))
      (SETQ RESULT
              (TAYSIMPLOG
               (LIST 'LOG
                     (ADDTAYLOR
                      (EXPTTAYRAT (ADDTAYLOR (MULTTAYLOR TAY TAY) TAY1)
                       (CONS 1 2))
                      TAY))))
      (COND
       ((MEMQ FN '(ASIN ACOS ASEC ACSC))
        (SETQ RESULT (MULTSQ RESULT (INVSQ (SIMP 'I))))))
      (RETURN (TAYSIMPSQ* RESULT)))) 
(PUT 'ASIN 'TAYLORSIMPFN 'TAYSIMPASIN) 
(PUT 'ACOS 'TAYLORSIMPFN 'TAYSIMPASIN) 
(PUT 'ASEC 'TAYLORSIMPFN 'TAYSIMPASIN) 
(PUT 'ACSC 'TAYLORSIMPFN 'TAYSIMPASIN) 
(PUT 'ASINH 'TAYLORSIMPFN 'TAYSIMPASIN) 
(PUT 'ACOSH 'TAYLORSIMPFN 'TAYSIMPASIN) 
(PUT 'ASECH 'TAYLORSIMPFN 'TAYSIMPASIN) 
(PUT 'ACSCH 'TAYLORSIMPFN 'TAYSIMPASIN) 
(PUT 'TAYSIMPATAN 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPATAN 'DEFINED-ON-LINE '508) 
(PUT 'TAYSIMPATAN 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPATAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPATAN (U)
    (COND
     ((OR (NOT (MEMQ (CAR U) '(ATAN ACOT ATANH ACOTH))) (CDDR U))
      (CONFUSION 'TAYSIMPATAN))
     (T
      (PROG (L L0 C0 V TAY TAY2 TP SINGLIST)
        (SETQ TAY (TAYSIMPSQ (SIMP* (CADR U))))
        (COND
         ((NOT (AND (KERNP TAY) (EQCAR (CAAAR (CAR TAY)) 'TAYLOR*)))
          (RETURN (MKSQ (LIST (CAR U) (MK*SQ TAY)) 1))))
        (SETQ TAY (CAAAR (CAR TAY)))
        (SETQ TP (CADDR TAY))
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
        (SETQ L
                ((LAMBDA (CFLIS)
                   (PROGN
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND (NOT (NULL CFLIS))
                              (NULL (CAR (CDR (CAR CFLIS))))))
                        (RETURN NIL)))
                      (SETQ CFLIS (CDR CFLIS))
                      (GO WHILELABEL))
                    CFLIS))
                 (CADR TAY)))
        (COND
         ((NULL L)
          (RETURN
           (CONS
            (LIST
             (CONS
              (GETPOWER
               (FKERN
                (LIST 'TAYLOR*
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
                        (SIMP* (LIST (CAR U) 0))))
                      TP
                      (COND ((NULL (CADDDR TAY)) NIL)
                            (T (SIMP* (LIST (CAR U) (PREPSQ (CADDDR TAY))))))
                      NIL))
               1)
              1))
            1))))
        (COND ((IS-NEG-PL (CAR (CAR L))) (RETURN (TAYSIMPATAN* (CAR U) TAY))))
        (SETQ C0 (GET-CST-COEFF TAY))
        (COND
         ((MEMQ (CAR U) '(ATAN ACOT))
          (SETQ C0 (SUBS2* (MULTSQ C0 (SIMP 'I))))))
        (COND
         ((OR (EQUAL C0 (CONS 1 1)) (EQUAL C0 (CONS (MINUS 1) 1)))
          (RETURN (TAYSIMPATAN* (CAR U) TAY))))
        (SETQ TAY2 (MULTTAYLOR TAY TAY))
        (COND ((MEMQ (CAR U) '(ATANH ACOTH)) (SETQ TAY2 (NEGTAYLOR TAY2))))
        (SETQ TAY2
                (INVTAYLOR
                 (ADDTAYLOR
                  ((LAMBDA (G175)
                     (LIST 'TAYLOR*
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
                                        ((LAMBDA (EL)
                                           (NLIST 0 (LENGTH (CAR EL))))
                                         (CAR EL))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL))
                             G175))
                           TP G175 NIL))
                   (CONS 1 1))
                  TAY2)))
        (COND ((EQ (CAR U) 'ACOT) (SETQ TAY2 (NEGTAYLOR TAY2))))
        (SETQ L
                (PROG (KRNL FORALL-RESULT FORALL-ENDPTR)
                  (SETQ KRNL
                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                            (SETQ X (CADDR TAY))
                           STARTOVER
                            (COND ((NULL X) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    ((LAMBDA (X) (APPEND (CAR X) NIL))
                                     (CAR X)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                            (SETQ X (CDR X))
                            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                           LOOPLABEL
                            (COND ((NULL X) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    ((LAMBDA (X) (APPEND (CAR X) NIL))
                                     (CAR X)))
                            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                            (SETQ X (CDR X))
                            (GO LOOPLABEL)))
                  (COND ((NULL KRNL) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (KRNL)
                                      (INTTAYLORWRTTAYVAR
                                       (MULTTAYLOR
                                        (DIFFTAYLORWRTTAYVAR TAY KRNL) TAY2)
                                       KRNL))
                                    (CAR KRNL))
                                   NIL)))
                 LOOPLABEL
                  (SETQ KRNL (CDR KRNL))
                  (COND ((NULL KRNL) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (KRNL)
                              (INTTAYLORWRTTAYVAR
                               (MULTTAYLOR (DIFFTAYLORWRTTAYVAR TAY KRNL) TAY2)
                               KRNL))
                            (CAR KRNL))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ V
                (TAYCOEFFLIST-UNION
                 (PROG (PP FORALL-RESULT FORALL-ENDPTR)
                   (SETQ PP L)
                   (COND ((NULL PP) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (PP) (CADR (CDR PP))) (CAR PP))
                                    NIL)))
                  LOOPLABEL
                   (SETQ PP (CDR PP))
                   (COND ((NULL PP) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (PP) (CADR (CDR PP))) (CAR PP)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
        (SETQ SINGLIST (CONS NIL 1))
        (PROG (PP)
          (SETQ PP L)
         LAB
          (COND ((NULL PP) (RETURN NIL)))
          ((LAMBDA (PP)
             (COND ((CAR PP) (SETQ SINGLIST (ADDSQ SINGLIST (CAR PP))))))
           (CAR PP))
          (SETQ PP (CDR PP))
          (GO LAB))
        (SETQ C0
                (SIMP
                 (LIST (CAR U)
                       (MK*SQ
                        ((LAMBDA (CC)
                           (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                         (ASSOC L0 (CADR TAY)))))))
        (SETQ V (CONS (CONS L0 C0) V))
        (SETQ TAY
                (LIST 'TAYLOR* V TP
                      (COND
                       ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
                        (SIMP (LIST (CAR U) (MK*SQ (CADDDR TAY)))))
                       (T NIL))
                      (CAR (CDDDDR TAY))))
        (COND
         ((NULL (CAR SINGLIST))
          (RETURN (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1))))
        (COND
         ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
          (RPLACA (CDDDR TAY) (ADDSQ (CADDDR TAY) (NEGSQ SINGLIST)))))
        (RETURN
         (ADDSQ SINGLIST (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1))))))) 
(PUT 'TAYSIMPATAN* 'NUMBER-OF-ARGS 2) 
(PUT 'TAYSIMPATAN* 'DEFINED-ON-LINE '562) 
(PUT 'TAYSIMPATAN* 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPATAN* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYSIMPATAN* (FN TAY)
    (PROG (RESULT TAY1 FLG)
      (COND ((MEMQ FN '(ATAN ACOT)) (SETQ TAY (MULTTAYLORSQ TAY (SIMP 'I)))))
      (SETQ TAY1
              ((LAMBDA (G177 G178)
                 (LIST 'TAYLOR*
                       (LIST
                        (CONS
                         (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                           (SETQ EL G178)
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
                         G177))
                       G178 G177 NIL))
               (CONS 1 1) (CADDR TAY)))
      (SETQ TAY
              (QUOTTAYLOR (ADDTAYLOR TAY1 TAY)
               (ADDTAYLOR TAY1 (NEGTAYLOR TAY))))
      (COND ((EQUAL (GET-CST-COEFF TAY) '(-1 . 1)) (SETQ FLG T)))
      (SETQ RESULT (TAYSIMPLOG (LIST 'LOG TAY)))
      (COND
       ((AND FLG (AND (KERNP RESULT) (EQCAR (CAAAR (CAR RESULT)) 'TAYLOR*)))
        ((LAMBDA (TAY)
           (RPLACD
            (ASSOC
             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
               (SETQ EL (CADDR TAY))
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
               (GO LOOPLABEL))
             (CADR TAY))
            (SIMP '(TIMES I PI))))
         (CAAAR (CAR RESULT)))))
      (SETQ RESULT (MULTSQ RESULT (CONS 1 2)))
      (COND ((EQ FN 'ATAN) (SETQ RESULT (MULTSQ RESULT (INVSQ (SIMP 'I)))))
            ((EQ FN 'ACOT) (SETQ RESULT (MULTSQ RESULT (SIMP 'I)))))
      (RETURN (TAYSIMPSQ* RESULT)))) 
(PUT 'ATAN 'TAYLORSIMPFN 'TAYSIMPATAN) 
(PUT 'ACOT 'TAYLORSIMPFN 'TAYSIMPATAN) 
(PUT 'ATANH 'TAYLORSIMPFN 'TAYSIMPATAN) 
(PUT 'ACOTH 'TAYLORSIMPFN 'TAYSIMPATAN) 
(PUT 'TAYSIMPLOG 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPLOG 'DEFINED-ON-LINE '662) 
(PUT 'TAYSIMPLOG 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPLOG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPLOG (U)
    (COND ((OR (NOT (EQ (CAR U) 'LOG)) (CDDR U)) (CONFUSION 'TAYSIMPLOG))
          (T
           (PROG (A0 CLIST COEFFLIS IL L0 L TAY TP CSING SINGTERM RESULT)
             ((LAMBDA (G180)
                (COND
                 (*TRTAYLOR
                  (PROGN
                   (LPRI
                    (CONS "Taylor trace (level"
                          (CONS
                           (COMPRESS
                            (CONS '|"|
                                  (NCONC (EXPLODE2 *TAYLOR-EXPANSION-LEVEL*)
                                         '(|)| |:| |"|))))
                           (COND ((AND G180 (ATOM G180)) (LIST G180))
                                 (T G180)))))
                   (TERPRI)))))
              (LIST "computing log of"))
             (COND (*TRTAYLOR (MATHPRINT U)))
             (SETQ U (SIMPLOGI (CADR U)))
             (COND ((NOT (KERNP U)) (RETURN (TAYSIMPSQ U))))
             (SETQ U (CAAAR (CAR U)))
             (COND ((NOT (EQ (CAR U) 'LOG)) (CONFUSION 'TAYSIMPLOG)))
             (SETQ U (TAYSIMPSQ (SIMP* (CADR U))))
             (COND
              ((NOT (AND (KERNP U) (EQCAR (CAAAR (CAR U)) 'TAYLOR*)))
               (RETURN (MKSQ (LIST 'LOG (MK*SQ U)) 1))))
             (SETQ TAY (CAAAR (CAR U)))
             (SETQ TP (CADDR TAY))
             (SETQ L0
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
                       (GO LOOPLABEL)))
             (SETQ L
                     ((LAMBDA (CFLIS)
                        (PROGN
                         (PROG ()
                          WHILELABEL
                           (COND
                            ((NOT
                              (AND (NOT (NULL CFLIS))
                                   (NULL (CAR (CDR (CAR CFLIS))))))
                             (RETURN NIL)))
                           (SETQ CFLIS (CDR CFLIS))
                           (GO WHILELABEL))
                         CFLIS))
                      (CADR TAY)))
             (COND ((NULL L) (TAYLOR-ERROR* 'NOT-A-UNIT 'TAYSIMPLOG)))
             (COND
              ((NEQ (CAR (CAR L)) L0)
               (PROGN
                (SETQ CSING (CAR (CAR L)))
                (SETQ L
                        (PROG (PP FORALL-RESULT FORALL-ENDPTR)
                          (SETQ PP L)
                          (COND ((NULL PP) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (PP)
                                              (PROG (NEWPL)
                                                (SETQ NEWPL
                                                        (SUBTR-DEGREES (CAR PP)
                                                         CSING))
                                                (COND
                                                 ((IS-NEG-PL NEWPL)
                                                  (TAYLOR-ERROR 'NOT-A-UNIT
                                                   'TAYSIMPLOG))
                                                 (T
                                                  (RETURN
                                                   (CONS NEWPL (CDR PP)))))))
                                            (CAR PP))
                                           NIL)))
                         LOOPLABEL
                          (SETQ PP (CDR PP))
                          (COND ((NULL PP) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (PP)
                                      (PROG (NEWPL)
                                        (SETQ NEWPL
                                                (SUBTR-DEGREES (CAR PP) CSING))
                                        (COND
                                         ((IS-NEG-PL NEWPL)
                                          (TAYLOR-ERROR 'NOT-A-UNIT
                                           'TAYSIMPLOG))
                                         (T (RETURN (CONS NEWPL (CDR PP)))))))
                                    (CAR PP))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                (SETQ TP
                        (ADDTO-ALL-TAYTPELORDERS TP
                         (PROG (NL FORALL-RESULT FORALL-ENDPTR)
                           (SETQ NL CSING)
                           (COND ((NULL NL) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (NL)
                                               (TAYEXP-MINUS
                                                (PROG (N FORALL-RESULT)
                                                  (SETQ N NL)
                                                  (SETQ FORALL-RESULT 0)
                                                 LAB1
                                                  (COND
                                                   ((NULL N)
                                                    (RETURN FORALL-RESULT)))
                                                  (SETQ FORALL-RESULT
                                                          (TAYEXP-PLUS
                                                           ((LAMBDA (N) N)
                                                            (CAR N))
                                                           FORALL-RESULT))
                                                  (SETQ N (CDR N))
                                                  (GO LAB1))))
                                             (CAR NL))
                                            NIL)))
                          LOOPLABEL
                           (SETQ NL (CDR NL))
                           (COND ((NULL NL) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (NL)
                                       (TAYEXP-MINUS
                                        (PROG (N FORALL-RESULT)
                                          (SETQ N NL)
                                          (SETQ FORALL-RESULT 0)
                                         LAB1
                                          (COND
                                           ((NULL N) (RETURN FORALL-RESULT)))
                                          (SETQ FORALL-RESULT
                                                  (TAYEXP-PLUS
                                                   ((LAMBDA (N) N) (CAR N))
                                                   FORALL-RESULT))
                                          (SETQ N (CDR N))
                                          (GO LAB1))))
                                     (CAR NL))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
                (SETQ SINGTERM (SIMP* (RETIMES (PREPTAYCOEFF CSING TP))))
                (COND
                 ((|:MINUSP| (LNC (CAR (CDR (CAR L)))))
                  (PROGN
                   (SETQ SINGTERM (NEGSQ SINGTERM))
                   (SETQ L (NEGTAYLOR1 L))))))))
             (SETQ A0
                     ((LAMBDA (CC)
                        (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                      (ASSOC L0 L)))
             (SETQ CLIST (LIST (CONS L0 (SIMPLOGI (MK*SQ A0)))))
             (SETQ IL
                     (COND ((NULL L) (NLIST 1 (LENGTH TP)))
                           (T (SMALLEST-INCREMENT L))))
             (SETQ COEFFLIS
                     (MAKECOEFFS0 TP
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X TP)
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (X) (CADDDR X)) (CAR X))
                                              NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADDDR X)) (CAR X)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      IL))
             (COND
              ((NOT (NULL COEFFLIS))
               (PROG (CC)
                 (SETQ CC (CDR COEFFLIS))
                LAB
                 (COND ((NULL CC) (RETURN NIL)))
                 ((LAMBDA (CC)
                    (PROG (S POS PP N N1)
                      (SETQ S (CONS NIL 1))
                      (SETQ POS (FIND-NON-ZERO CC))
                      (SETQ N (NTH (NTH CC (CAR POS)) (CDR POS)))
                      (SETQ PP (MAKECOEFFPAIRS L0 CC (CAR (CAR L)) IL))
                      (PROG (P)
                        (SETQ P PP)
                       LAB
                        (COND ((NULL P) (RETURN NIL)))
                        ((LAMBDA (P)
                           (PROGN
                            (SETQ N1 (NTH (NTH (CAR P) (CAR POS)) (CDR POS)))
                            (SETQ S
                                    (ADDSQ S
                                           (MULTSQ (*TAYEXP2Q N1)
                                                   (MULTSQ
                                                    ((LAMBDA (CC)
                                                       (COND
                                                        ((NULL CC)
                                                         (CONS NIL 1))
                                                        (T (CDR CC))))
                                                     (ASSOC (CAR P) CLIST))
                                                    ((LAMBDA (CC)
                                                       (COND
                                                        ((NULL CC)
                                                         (CONS NIL 1))
                                                        (T (CDR CC))))
                                                     (ASSOC (CDR P) L))))))))
                         (CAR P))
                        (SETQ P (CDR P))
                        (GO LAB))
                      (SETQ S
                              (ADDSQ
                               ((LAMBDA (CC)
                                  (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                                (ASSOC CC L))
                               (NEGSQ (MULTSQ S (INVSQ (*TAYEXP2Q N))))))
                      (COND
                       ((NOT (NULL (CAR S)))
                        (SETQ CLIST
                                (CONS (CONS CC (MULTSQ S (INVSQ A0)))
                                      CLIST))))))
                  (CAR CC))
                 (SETQ CC (CDR CC))
                 (GO LAB))))
             (SETQ TAY
                     (LIST 'TAYLOR* (REVERSIP CLIST) TP
                           (COND
                            ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
                             (SIMPLOGI (MK*SQ (CADDDR TAY))))
                            (T NIL))
                           (CAR (CDDDDR TAY))))
             (COND
              ((NULL CSING)
               (RETURN (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1)) 1))))
             (SETQ SINGTERM (SIMPLOGSQ SINGTERM))
             (COND ((TAYLOR*-ZEROP TAY) (RETURN SINGTERM)))
             (COND
              ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
               (RPLACA (CDDDR TAY) (ADDSQ (CADDDR TAY) (NEGSQ SINGTERM)))))
             (SETQ RESULT
                     (TAYSIMPSQ
                      (ADDSQ SINGTERM
                             (CONS (LIST (CONS (GETPOWER (FKERN TAY) 1) 1))
                                   1))))
             ((LAMBDA (G182)
                (COND
                 (*TRTAYLOR
                  (PROGN
                   (LPRI
                    (CONS "Taylor trace (level"
                          (CONS
                           (COMPRESS
                            (CONS '|"|
                                  (NCONC (EXPLODE2 *TAYLOR-EXPANSION-LEVEL*)
                                         '(|)| |:| |"|))))
                           (COND ((AND G182 (ATOM G182)) (LIST G182))
                                 (T G182)))))
                   (TERPRI)))))
              (LIST "result of computing log is"))
             (COND (*TRTAYLOR (MATHPRINT (MK*SQ RESULT))))
             (RETURN RESULT))))) 
(PUT 'LOG 'TAYLORSIMPFN 'TAYSIMPLOG) 
(PUT 'TAYSIMPEXP 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPEXP 'DEFINED-ON-LINE '758) 
(PUT 'TAYSIMPEXP 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPEXP (U)
    (COND ((OR (NOT (EQ (CAR U) 'EXP)) (CDDR U)) (CONFUSION 'TAYSIMPEXP))
          (T
           (PROG (A0 CLIST COEFFLIS IL L0 L LM LP TAY TP)
             (SETQ U (TAYSIMPSQ (SIMP* (CADR U))))
             (COND
              ((NOT (AND (KERNP U) (EQCAR (CAAAR (CAR U)) 'TAYLOR*)))
               (RETURN (MKSQ (LIST 'EXP (MK*SQ U)) 1))))
             (SETQ TAY (CAAAR (CAR U)))
             (SETQ TP (CADDR TAY))
             (SETQ L0
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
                       (GO LOOPLABEL)))
             (SETQ L
                     ((LAMBDA (CFLIS)
                        (PROGN
                         (PROG ()
                          WHILELABEL
                           (COND
                            ((NOT
                              (AND (NOT (NULL CFLIS))
                                   (NULL (CAR (CDR (CAR CFLIS))))))
                             (RETURN NIL)))
                           (SETQ CFLIS (CDR CFLIS))
                           (GO WHILELABEL))
                         CFLIS))
                      (CADR TAY)))
             (COND
              ((NULL L)
               (RETURN
                (CONS
                 (LIST
                  (CONS
                   (GETPOWER
                    (FKERN
                     ((LAMBDA (G183)
                        (LIST 'TAYLOR*
                              (LIST
                               (CONS
                                (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ EL TP)
                                  (COND ((NULL EL) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (EL)
                                                      (NLIST 0
                                                             (LENGTH
                                                              (CAR EL))))
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
                                G183))
                              TP G183 NIL))
                      (CONS 1 1)))
                    1)
                   1))
                 1))))
             (PROG (PP)
               (SETQ PP L)
              LAB
               (COND ((NULL PP) (RETURN NIL)))
               ((LAMBDA (PP)
                  (COND ((IS-NEG-PL (CAR PP)) (SETQ LM (CONS PP LM)))
                        ((NOT (NULL (CAR (CDR PP)))) (SETQ LP (CONS PP LP)))))
                (CAR PP))
               (SETQ PP (CDR PP))
               (GO LAB))
             (SETQ LM (REVERSIP LM))
             (SETQ L (REVERSIP LP))
             (COND
              (LM
               (SETQ LM
                       (SIMP*
                        (LIST 'EXP
                              (PREPTAYLOR* (LIST 'TAYLOR* LM TP NIL NIL)))))))
             (COND ((NULL L) (RETURN LM)))
             (SETQ A0
                     ((LAMBDA (CC)
                        (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                      (ASSOC L0 L)))
             (SETQ CLIST (LIST (CONS L0 (SIMP* (LIST 'EXP (MK*SQ A0))))))
             (SETQ IL (SMALLEST-INCREMENT L))
             (SETQ COEFFLIS
                     (MAKECOEFFS0 TP
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X TP)
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (X) (CADDDR X)) (CAR X))
                                              NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADDDR X)) (CAR X)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))
                      IL))
             (COND
              ((NOT (NULL COEFFLIS))
               (PROG (CC)
                 (SETQ CC (CDR COEFFLIS))
                LAB
                 (COND ((NULL CC) (RETURN NIL)))
                 ((LAMBDA (CC)
                    (PROG (S POS PP N N1)
                      (SETQ S (CONS NIL 1))
                      (SETQ POS (FIND-NON-ZERO CC))
                      (SETQ N (NTH (NTH CC (CAR POS)) (CDR POS)))
                      (SETQ PP (MAKECOEFFPAIRS L0 CC L0 IL))
                      (PROG (P)
                        (SETQ P PP)
                       LAB
                        (COND ((NULL P) (RETURN NIL)))
                        ((LAMBDA (P)
                           (PROGN
                            (SETQ N1 (NTH (NTH (CAR P) (CAR POS)) (CDR POS)))
                            (SETQ S
                                    (ADDSQ S
                                           (MULTSQ
                                            (*TAYEXP2Q
                                             (TAYEXP-DIFFERENCE N N1))
                                            (MULTSQ
                                             ((LAMBDA (CC)
                                                (COND ((NULL CC) (CONS NIL 1))
                                                      (T (CDR CC))))
                                              (ASSOC (CAR P) CLIST))
                                             ((LAMBDA (CC)
                                                (COND ((NULL CC) (CONS NIL 1))
                                                      (T (CDR CC))))
                                              (ASSOC (CDR P) L))))))))
                         (CAR P))
                        (SETQ P (CDR P))
                        (GO LAB))
                      (SETQ S (SUBS2* (MULTSQ S (INVSQ (*TAYEXP2Q N)))))
                      (COND
                       ((NOT (NULL (CAR S)))
                        (SETQ CLIST (CONS (CONS CC S) CLIST))))))
                  (CAR CC))
                 (SETQ CC (CDR CC))
                 (GO LAB))))
             (SETQ CLIST (REVERSIP CLIST))
             (SETQ U
                     (CONS
                      (LIST
                       (CONS
                        (GETPOWER
                         (FKERN
                          (LIST 'TAYLOR* CLIST TP
                                (COND
                                 ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
                                  (SIMP (LIST 'EXP (MK*SQ (CADDDR TAY)))))
                                 (T NIL))
                                (CAR (CDDDDR TAY))))
                         1)
                        1))
                      1))
             (RETURN (COND ((NULL LM) U) (T (MULTSQ U LM)))))))) 
(PUT 'EXP 'TAYLORSIMPFN 'TAYSIMPEXP) 
(SETQ **TAYLOR-EPSILON** '~TAYEPS~) 
(PUT 'TAYSIMPTAN 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPTAN 'DEFINED-ON-LINE '1000) 
(PUT 'TAYSIMPTAN 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPTAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPTAN (U)
    (COND
     ((OR (NOT (MEMQ (CAR U) '(TAN COT TANH COTH))) (CDDR U))
      (CONFUSION 'TAYSIMPTAN))
     (T
      (PROG (A A0 CLIST COEFFLIS IL L0 L POLEFLAG TAY TP)
        (SETQ TAY (TAYSIMPSQ (SIMP* (CADR U))))
        (COND
         ((NOT (AND (KERNP TAY) (EQCAR (CAAAR (CAR TAY)) 'TAYLOR*)))
          (RETURN (MKSQ (LIST (CAR U) (MK*SQ TAY)) 1))))
        (SETQ TAY (CAAAR (CAR TAY)))
        (SETQ TP (CADDR TAY))
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
        (SETQ L
                ((LAMBDA (CFLIS)
                   (PROGN
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND (NOT (NULL CFLIS))
                              (NULL (CAR (CDR (CAR CFLIS))))))
                        (RETURN NIL)))
                      (SETQ CFLIS (CDR CFLIS))
                      (GO WHILELABEL))
                    CFLIS))
                 (CADR TAY)))
        (COND
         ((AND (NOT (NULL L)) (IS-NEG-PL (CAR (CAR L))))
          (TAYLOR-ERROR 'ESSENTIAL-SINGULARITY (CAR U))))
        (SETQ A0
                ((LAMBDA (CC) (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                 (ASSOC L0 L)))
        (SETQ IL
                (COND ((NULL L) (NLIST 1 (LENGTH TP)))
                      (T (SMALLEST-INCREMENT L))))
        (SETQ A (MULTSQ A0 (INVSQ (SIMP 'PI))))
        (COND
         ((MEMQ (CAR U) '(TANH COTH)) (SETQ A (SUBS2* (MULTSQ A (SIMP 'I))))))
        (COND
         ((OR
           (AND (MEMQ (CAR U) '(TAN TANH))
                (EQUAL (CDR (SETQ A (MULTSQ A (SIMP '2)))) 1)
                (FIXP (SETQ A (CAR A))) (NOT (EVENP A)))
           (AND (MEMQ (CAR U) '(COT COTH)) (EQUAL (CDR A) 1)
                (OR (NULL (SETQ A (CAR A))) (FIXP A))))
          (PROGN
           (SETQ POLEFLAG T)
           (SETQ A0
                   (COND
                    ((EQ (CAR U) 'TAN)
                     (NEGSQ (INVSQ (SIMP* (LIST 'TAN **TAYLOR-EPSILON**)))))
                    ((EQ (CAR U) 'TANH)
                     (INVSQ (SIMP* (LIST 'TANH **TAYLOR-EPSILON**))))
                    ((EQ (CAR U) 'COT)
                     (INVSQ (SIMP* (LIST 'TAN **TAYLOR-EPSILON**))))
                    (T (INVSQ (SIMP* (LIST 'TANH **TAYLOR-EPSILON**))))))
           (SETQ CLIST (LIST (CONS L0 A0)))
           NIL))
         (T (SETQ CLIST (LIST (CONS L0 (SIMP* (LIST (CAR U) (MK*SQ A0))))))))
        (SETQ COEFFLIS
                (MAKECOEFFS0 TP
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X TP)
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (X) (CADDDR X)) (CAR X))
                                         NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (CADDDR X)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 IL))
        (COND
         ((NOT (NULL COEFFLIS))
          (PROG (CC)
            (SETQ CC (CDR COEFFLIS))
           LAB
            (COND ((NULL CC) (RETURN NIL)))
            ((LAMBDA (CC)
               (PROG (CF S POS X Y N N1)
                 (SETQ S (CONS NIL 1))
                 (SETQ POS (FIND-NON-ZERO CC))
                 (SETQ N (NTH (NTH CC (CAR POS)) (CDR POS)))
                 (PROG (P)
                   (SETQ P (MAKECOEFFPAIRS L0 CC L0 IL))
                  LAB
                   (COND ((NULL P) (RETURN NIL)))
                   ((LAMBDA (P)
                      (PROGN
                       (SETQ X (REVERSIP (MAKECOEFFPAIRS1 L0 (CAR P) L0 IL)))
                       (SETQ Y (CONS NIL 1))
                       (PROG (Z)
                         (SETQ Z X)
                        LAB
                         (COND ((NULL Z) (RETURN NIL)))
                         ((LAMBDA (Z)
                            (SETQ Y
                                    (ADDSQ Y
                                           (MULTSQ
                                            ((LAMBDA (CC)
                                               (COND ((NULL CC) (CONS NIL 1))
                                                     (T (CDR CC))))
                                             (ASSOC (CAR Z) CLIST))
                                            ((LAMBDA (CC)
                                               (COND ((NULL CC) (CONS NIL 1))
                                                     (T (CDR CC))))
                                             (ASSOC (CDR Z) CLIST))))))
                          (CAR Z))
                         (SETQ Z (CDR Z))
                         (GO LAB))
                       (SETQ N1 (NTH (NTH (CAR P) (CAR POS)) (CDR POS)))
                       (SETQ S
                               (ADDSQ S
                                      (MULTSQ
                                       (*TAYEXP2Q (TAYEXP-DIFFERENCE N N1))
                                       (MULTSQ Y
                                               ((LAMBDA (CC)
                                                  (COND
                                                   ((NULL CC) (CONS NIL 1))
                                                   (T (CDR CC))))
                                                (ASSOC (CDR P) L))))))))
                    (CAR P))
                   (SETQ P (CDR P))
                   (GO LAB))
                 (SETQ CF (MULTSQ S (INVSQ (*TAYEXP2Q N))))
                 (COND ((MEMQ (CAR U) '(TANH COTH)) (SETQ CF (NEGSQ CF))))
                 (SETQ CF
                         (ADDSQ
                          ((LAMBDA (CC)
                             (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                           (ASSOC CC L))
                          CF))
                 (COND ((NULL (CAR CF)) (RETURN NIL)))
                 (COND ((EQ (CAR U) 'COT) (SETQ CF (NEGSQ CF))))
                 (SETQ CLIST (CONS (CONS CC CF) CLIST))))
             (CAR CC))
            (SETQ CC (CDR CC))
            (GO LAB))))
        (SETQ A (LIST 'TAYLOR* (REVERSIP CLIST) TP NIL NIL))
        (COND
         (POLEFLAG
          (PROG (X)
            (SETQ X
                    (COND
                     ((EQ (CAR U) 'COT)
                      ((LAMBDA (G185)
                         (LIST 'TAYLOR*
                               (LIST
                                (CONS
                                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ EL TP)
                                   (COND ((NULL EL) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (EL)
                                                       (NLIST 0
                                                              (LENGTH
                                                               (CAR EL))))
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
                                 G185))
                               TP G185 NIL))
                       (INVSQ (SIMP (LIST 'TAN **TAYLOR-EPSILON**)))))
                     ((EQ (CAR U) 'COTH)
                      ((LAMBDA (G187)
                         (LIST 'TAYLOR*
                               (LIST
                                (CONS
                                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ EL TP)
                                   (COND ((NULL EL) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (EL)
                                                       (NLIST 0
                                                              (LENGTH
                                                               (CAR EL))))
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
                                 G187))
                               TP G187 NIL))
                       (INVSQ (SIMP (LIST 'TANH **TAYLOR-EPSILON**)))))
                     (T
                      ((LAMBDA (G189)
                         (LIST 'TAYLOR*
                               (LIST
                                (CONS
                                 (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ EL TP)
                                   (COND ((NULL EL) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (EL)
                                                       (NLIST 0
                                                              (LENGTH
                                                               (CAR EL))))
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
                                 G189))
                               TP G189 NIL))
                       (SIMP (LIST (CAR U) **TAYLOR-EPSILON**))))))
            (COND
             ((EQ (CAR U) 'TAN)
              (SETQ A
                      (QUOTTAYLOR (ADDTAYLOR A (NEGTAYLOR X))
                       (ADDTAYLOR
                        ((LAMBDA (G191)
                           (LIST 'TAYLOR*
                                 (LIST
                                  (CONS
                                   (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ EL TP)
                                     (COND ((NULL EL) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (EL)
                                                         (NLIST 0
                                                                (LENGTH
                                                                 (CAR EL))))
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
                                   G191))
                                 TP G191 NIL))
                         (CONS 1 1))
                        (MULTTAYLOR A X)))))
             ((EQ (CAR U) 'COT)
              (SETQ A
                      (QUOTTAYLOR
                       (ADDTAYLOR (MULTTAYLOR A X)
                        ((LAMBDA (G193)
                           (LIST 'TAYLOR*
                                 (LIST
                                  (CONS
                                   (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ EL TP)
                                     (COND ((NULL EL) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (EL)
                                                         (NLIST 0
                                                                (LENGTH
                                                                 (CAR EL))))
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
                                   G193))
                                 TP G193 NIL))
                         (CONS 1 1)))
                       (ADDTAYLOR X (NEGTAYLOR A)))))
             ((EQ (CAR U) 'TANH)
              (SETQ A
                      (QUOTTAYLOR (ADDTAYLOR A (NEGTAYLOR X))
                       (ADDTAYLOR
                        ((LAMBDA (G195)
                           (LIST 'TAYLOR*
                                 (LIST
                                  (CONS
                                   (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ EL TP)
                                     (COND ((NULL EL) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (EL)
                                                         (NLIST 0
                                                                (LENGTH
                                                                 (CAR EL))))
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
                                   G195))
                                 TP G195 NIL))
                         (CONS 1 1))
                        (NEGTAYLOR (MULTTAYLOR A X))))))
             ((EQ (CAR U) 'COTH)
              (SETQ A
                      (QUOTTAYLOR
                       (ADDTAYLOR (MULTTAYLOR A X)
                        ((LAMBDA (G197)
                           (LIST 'TAYLOR*
                                 (LIST
                                  (CONS
                                   (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ EL TP)
                                     (COND ((NULL EL) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (EL)
                                                         (NLIST 0
                                                                (LENGTH
                                                                 (CAR EL))))
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
                                   G197))
                                 TP G197 NIL))
                         (CONS (TAYEXP-MINUS 1) 1)))
                       (ADDTAYLOR X (NEGTAYLOR A))))))
            (COND
             ((NOT (FREEOF A **TAYLOR-EPSILON**))
              (RPLACA (CDR A)
                      (PROG (PP FORALL-RESULT FORALL-ENDPTR)
                        (SETQ PP (CADR A))
                        (COND ((NULL PP) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (PP)
                                            (CONS (CAR PP)
                                                  (SUBSQ (CDR PP)
                                                         (LIST
                                                          (CONS
                                                           **TAYLOR-EPSILON**
                                                           0)))))
                                          (CAR PP))
                                         NIL)))
                       LOOPLABEL
                        (SETQ PP (CDR PP))
                        (COND ((NULL PP) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (PP)
                                    (CONS (CAR PP)
                                          (SUBSQ (CDR PP)
                                                 (LIST
                                                  (CONS **TAYLOR-EPSILON**
                                                        0)))))
                                  (CAR PP))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))))))
        (COND
         ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
          (RPLACA (CDDDR A) (SIMP (LIST (CAR U) (MK*SQ (CADDDR TAY)))))))
        (RPLACA (CDDDDR A) (CAR (CDDDDR TAY)))
        (RETURN (CONS (LIST (CONS (GETPOWER (FKERN A) 1) 1)) 1)))))) 
(PUT 'TAN 'TAYLORSIMPFN 'TAYSIMPTAN) 
(PUT 'COT 'TAYLORSIMPFN 'TAYSIMPTAN) 
(PUT 'TANH 'TAYLORSIMPFN 'TAYSIMPTAN) 
(PUT 'COTH 'TAYLORSIMPFN 'TAYSIMPTAN) 
(PUT 'TAYSIMPSIN 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPSIN 'DEFINED-ON-LINE '1172) 
(PUT 'TAYSIMPSIN 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPSIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPSIN (U)
    (COND
     ((OR (NOT (MEMQ (CAR U) '(SIN COS SEC CSC))) (CDDR U))
      (CONFUSION 'TAYSIMPSIN))
     (T
      (PROG (L TAY RESULT RESULT2 TP I1 L0 A0 A1 A2)
        (SETQ TAY (TAYSIMPSQ (SIMP* (CADR U))))
        (COND
         ((NOT (AND (KERNP TAY) (EQCAR (CAAAR (CAR TAY)) 'TAYLOR*)))
          (RETURN (MKSQ (LIST (CAR U) (MK*SQ TAY)) 1))))
        (SETQ TAY (CAAAR (CAR TAY)))
        (SETQ TP (CADDR TAY))
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
        (SETQ L
                ((LAMBDA (CFLIS)
                   (PROGN
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND (NOT (NULL CFLIS))
                              (NULL (CAR (CDR (CAR CFLIS))))))
                        (RETURN NIL)))
                      (SETQ CFLIS (CDR CFLIS))
                      (GO WHILELABEL))
                    CFLIS))
                 (CADR TAY)))
        (SETQ A0
                ((LAMBDA (CC) (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                 (ASSOC L0 L)))
        (SETQ I1 (SIMP 'I))
        (COND
         ((NOT (NULL (CAR A0)))
          (SETQ TAY
                  (ADDTAYLOR TAY
                   ((LAMBDA (G199)
                      (LIST 'TAYLOR*
                            (LIST
                             (CONS
                              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL TP)
                                (COND ((NULL EL) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL)
                                                    (NLIST 0
                                                           (LENGTH (CAR EL))))
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
                              G199))
                            TP G199 NIL))
                    (NEGSQ A0))))))
        (SETQ RESULT
                (TAYSIMPEXP
                 (LIST 'EXP
                       (MULTTAYLOR TAY
                        (LIST 'TAYLOR*
                              (LIST
                               (CONS
                                (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                  (SETQ EL TP)
                                  (COND ((NULL EL) (RETURN NIL)))
                                  (SETQ FORALL-RESULT
                                          (SETQ FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (EL)
                                                      (NLIST 0
                                                             (LENGTH
                                                              (CAR EL))))
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
                                I1))
                              TP I1 NIL)))))
        (SETQ RESULT2
                (TAYSIMPEXP
                 (LIST 'EXP
                       (MULTTAYLOR TAY
                        ((LAMBDA (G201)
                           (LIST 'TAYLOR*
                                 (LIST
                                  (CONS
                                   (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ EL TP)
                                     (COND ((NULL EL) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (EL)
                                                         (NLIST 0
                                                                (LENGTH
                                                                 (CAR EL))))
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
                                   G201))
                                 TP G201 NIL))
                         (NEGSQ I1))))))
        (SETQ A1
                (CONS (SIMP* (LIST 'SIN (MK*SQ A0)))
                      (SIMP* (LIST 'COS (MK*SQ A0)))))
        (COND
         ((MEMQ (CAR U) '(SIN CSC))
          (PROGN
           (SETQ A2 (ADDSQ (CAR A1) (MULTSQ I1 (CDR A1))))
           (SETQ A1 (ADDSQ (CAR A1) (NEGSQ (MULTSQ I1 (CDR A1)))))
           NIL))
         (T
          (PROGN
           (SETQ A2 (ADDSQ (CDR A1) (NEGSQ (MULTSQ I1 (CAR A1)))))
           (SETQ A1 (ADDSQ (CDR A1) (MULTSQ I1 (CAR A1))))
           NIL)))
        (SETQ RESULT
                (MULTSQ (ADDSQ (MULTSQ RESULT A1) (MULTSQ RESULT2 A2))
                        (CONS 1 2)))
        (COND
         ((MEMQ (CAR U) '(SEC CSC))
          (PROGN
           (COND ((NULL (CAR RESULT)) (TAYLOR-ERROR* 'NOT-A-UNIT U))
                 (T (SETQ RESULT (INVSQ RESULT))))
           NIL))
         ((NULL (CAR RESULT))
          (RETURN
           (CONS
            (LIST
             (CONS
              (GETPOWER
               (FKERN
                (LIST 'TAYLOR*
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
                        RESULT))
                      TP RESULT NIL))
               1)
              1))
            1))))
        (RETURN (TAYSIMPSQ* RESULT)))))) 
(PUT 'SIN 'TAYLORSIMPFN 'TAYSIMPSIN) 
(PUT 'COS 'TAYLORSIMPFN 'TAYSIMPSIN) 
(PUT 'SEC 'TAYLORSIMPFN 'TAYSIMPSIN) 
(PUT 'CSC 'TAYLORSIMPFN 'TAYSIMPSIN) 
(PUT 'TAYSIMPSINH 'NUMBER-OF-ARGS 1) 
(PUT 'TAYSIMPSINH 'DEFINED-ON-LINE '1265) 
(PUT 'TAYSIMPSINH 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'TAYSIMPSINH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TAYSIMPSINH (U)
    (COND
     ((OR (NOT (MEMQ (CAR U) '(SINH COSH SECH CSCH))) (CDDR U))
      (CONFUSION 'TAYSIMPSIN))
     (T
      (PROG (L TAY RESULT RESULT2 TP L0 A0 A1 A2)
        (SETQ TAY (TAYSIMPSQ (SIMP* (CADR U))))
        (COND
         ((NOT (AND (KERNP TAY) (EQCAR (CAAAR (CAR TAY)) 'TAYLOR*)))
          (RETURN (MKSQ (LIST (CAR U) (MK*SQ TAY)) 1))))
        (SETQ TAY (CAAAR (CAR TAY)))
        (SETQ TP (CADDR TAY))
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
        (SETQ L
                ((LAMBDA (CFLIS)
                   (PROGN
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT
                         (AND (NOT (NULL CFLIS))
                              (NULL (CAR (CDR (CAR CFLIS))))))
                        (RETURN NIL)))
                      (SETQ CFLIS (CDR CFLIS))
                      (GO WHILELABEL))
                    CFLIS))
                 (CADR TAY)))
        (SETQ A0
                ((LAMBDA (CC) (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                 (ASSOC L0 L)))
        (COND
         ((NOT (NULL (CAR A0)))
          (SETQ TAY
                  (ADDTAYLOR TAY
                   ((LAMBDA (G203)
                      (LIST 'TAYLOR*
                            (LIST
                             (CONS
                              (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                (SETQ EL TP)
                                (COND ((NULL EL) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (EL)
                                                    (NLIST 0
                                                           (LENGTH (CAR EL))))
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
                              G203))
                            TP G203 NIL))
                    (NEGSQ A0))))))
        (SETQ RESULT (TAYSIMPEXP (LIST 'EXP TAY)))
        (SETQ RESULT2 (TAYSIMPEXP (LIST 'EXP (NEGTAYLOR TAY))))
        (SETQ A1
                (CONS (SIMP* (LIST 'SINH (MK*SQ A0)))
                      (SIMP* (LIST 'COSH (MK*SQ A0)))))
        (COND
         ((MEMQ (CAR U) '(SINH CSCH))
          (PROGN
           (SETQ A2 (ADDSQ (CAR A1) (CDR A1)))
           (SETQ A1 (ADDSQ (CAR A1) (NEGSQ (CDR A1))))
           NIL))
         (T
          (PROGN
           (SETQ A2 (ADDSQ (CDR A1) (CAR A1)))
           (SETQ A1 (ADDSQ (CDR A1) (NEGSQ (CAR A1))))
           NIL)))
        (SETQ RESULT
                (MULTSQ (ADDSQ (MULTSQ RESULT A2) (MULTSQ RESULT2 A1))
                        (CONS 1 2)))
        (COND
         ((MEMQ (CAR U) '(SECH CSCH))
          (PROGN
           (COND ((NULL (CAR RESULT)) (TAYLOR-ERROR* 'NOT-A-UNIT U))
                 (T (SETQ RESULT (INVSQ RESULT))))
           NIL))
         ((NULL (CAR RESULT))
          (RETURN
           (CONS
            (LIST
             (CONS
              (GETPOWER
               (FKERN
                (LIST 'TAYLOR*
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
                        RESULT))
                      TP RESULT NIL))
               1)
              1))
            1))))
        (RETURN (TAYSIMPSQ* RESULT)))))) 
(PUT 'SINH 'TAYLORSIMPFN 'TAYSIMPSINH) 
(PUT 'COSH 'TAYLORSIMPFN 'TAYSIMPSINH) 
(PUT 'SECH 'TAYLORSIMPFN 'TAYSIMPSINH) 
(PUT 'CSCH 'TAYLORSIMPFN 'TAYSIMPSINH) 
(AEVAL
 (LET
  (LIST
   (LIST 'LIST
         (LIST 'REPLACEBY
               (LIST 'INT
                     (AEVAL
                      (LIST 'TAYLOR* (LIST '~ 'X) (LIST '~ 'Y) (LIST '~ 'Z)
                            (LIST '~ 'W)))
                     (LIST '~ 'U))
               (LIST 'TAYLORINT (LIST '~ 'X) (LIST '~ 'Y) (LIST '~ 'Z)
                     (LIST '~ 'W) (LIST '~ 'U)))
         (LIST 'REPLACEBY
               (LIST 'INT
                     (LIST 'TIMES
                           (LIST 'EXPT (LIST 'LOG (LIST '~ 'U))
                                 (LIST '~ (LIST '~ 'N)))
                           (AEVAL
                            (LIST 'TAYLOR* (LIST '~ 'X) (LIST '~ 'Y)
                                  (LIST '~ 'Z) (LIST '~ 'W))))
                     (LIST '~ 'U))
               (LIST 'DIFFERENCE
                     (LIST 'TIMES (LIST 'EXPT (LIST 'LOG 'U) 'N)
                           (LIST 'INT '(TAYLOR* X Y Z W) 'U))
                     (LIST 'INT
                           (LIST 'TIMES
                                 (LIST 'EXPT (LIST 'LOG 'U)
                                       (LIST 'DIFFERENCE 'N 1))
                                 (LIST 'TAYLORCOMBINE
                                       (LIST 'QUOTIENT
                                             (LIST 'INT '(TAYLOR* X Y Z W) 'U)
                                             'U)))
                           'U)))
         (LIST 'REPLACEBY (LIST 'INT (LIST '~ 'X) (LIST '~ 'Y))
               (LIST 'WHEN (LIST 'TAYLORINT1 (LIST '~ 'X) (LIST '~ 'Y))
                     (LIST 'NOT
                           (LIST 'SYMBOLIC
                                 (LIST 'ALGEBRAIC
                                       (LIST 'FREEOF (LIST '~ 'X)
                                             ''TAYLOR*)))))))))) 
(PUT 'TAYLORINT1 'PSOPFN 'REVALTAYLORINT1) 
(PUT 'REVALTAYLORINT1 'NUMBER-OF-ARGS 1) 
(PUT 'REVALTAYLORINT1 'DEFINED-ON-LINE '1348) 
(PUT 'REVALTAYLORINT1 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'REVALTAYLORINT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REVALTAYLORINT1 (X)
    (PROG (U V)
      (SETQ U (CAR X))
      (SETQ V (CADR X))
      (COND
       ((EQCAR U 'TAYLOR*)
        (RETURN (REVALTAYLORINT (APPEND (CDR U) (LIST V))))))
      (SETQ U (REVAL1 (TAYLORCOMBINE U) T))
      (COND
       ((EQCAR U 'TAYLOR*)
        (RETURN (REVALTAYLORINT (APPEND (CDR U) (LIST V))))))
      (COND
       ((NOT (ATOM U))
        (COND
         ((MEMQ (CAR U) '(PLUS MINUS DIFFERENCE))
          (RETURN
           (REVAL1
            (CONS (CAR U)
                  (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                    (SETQ TERM (CDR U))
                    (COND ((NULL TERM) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (TERM) (LIST 'INT TERM V))
                                      (CAR TERM))
                                     NIL)))
                   LOOPLABEL
                    (SETQ TERM (CDR TERM))
                    (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (TERM) (LIST 'INT TERM V)) (CAR TERM))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
            T))))))
      (LPRIM "Converting Taylor kernels to standard representation")
      (RETURN (REVAL1 (LIST 'INT (TAYLORTOSTANDARD (CAR X)) V) NIL)))) 
(PUT 'TAYLORINT 'PSOPFN 'REVALTAYLORINT) 
(PUT 'REVALTAYLORINT 'NUMBER-OF-ARGS 1) 
(PUT 'REVALTAYLORINT 'DEFINED-ON-LINE '1366) 
(PUT 'REVALTAYLORINT 'DEFINED-IN-FILE 'TAYLOR/TAYFNS.RED) 
(PUT 'REVALTAYLORINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REVALTAYLORINT (U)
    (PROG (TAYCFL TAYTP TAYORG TAYFLGS VAR)
      (SETQ TAYCFL (CAR U))
      (SETQ TAYTP (CADR U))
      (SETQ TAYORG (CADDR U))
      (SETQ TAYFLGS (CADDDR U))
      (SETQ VAR (CAR (CDDDDR U)))
      (COND
       ((NULL
         (MEMBER VAR
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X TAYTP)
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
                   (GO LOOPLABEL))))
        (RETURN
         (MK*SQ
          (CONS
           (LIST
            (CONS
             (GETPOWER
              (FKERN
               (LIST 'TAYLOR*
                     (PROG (PP FORALL-RESULT FORALL-ENDPTR)
                       (SETQ PP TAYCFL)
                       (COND ((NULL PP) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (PP)
                                           (CONS (CAR PP)
                                                 (SIMP*
                                                  (LIST 'INT (MK*SQ (CDR PP))
                                                        VAR))))
                                         (CAR PP))
                                        NIL)))
                      LOOPLABEL
                       (SETQ PP (CDR PP))
                       (COND ((NULL PP) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (PP)
                                   (CONS (CAR PP)
                                         (SIMP*
                                          (LIST 'INT (MK*SQ (CDR PP)) VAR))))
                                 (CAR PP))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     TAYTP
                     (COND
                      ((NOT (NULL TAYORG))
                       (SIMP* (LIST 'INT (MK*SQ TAYORG) VAR)))
                      (T NIL))
                     NIL))
              1)
             1))
           1))))
       (T
        (RETURN
         (MK*SQ
          ((LAMBDA (RESULT)
             (COND
              ((NULL (CAR RESULT))
               (CONS (LIST (CONS (GETPOWER (FKERN (CDR RESULT)) 1) 1)) 1))
              (T
               (ADDSQ (CAR RESULT)
                      (CONS (LIST (CONS (GETPOWER (FKERN (CDR RESULT)) 1) 1))
                            1)))))
           (INTTAYLORWRTTAYVAR1 TAYCFL TAYTP VAR)))))))) 
(ENDMODULE) 