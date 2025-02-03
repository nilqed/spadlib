(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYEXPND)) 
(EXPORTS (LIST 'TAYLOREXPAND 'TAYLOREXPAND-DIFF)) 
(IMPORTS
 (LIST '*F2Q '*K2Q '*P2Q 'MULT 'ADD 'OVER 'AEVAL 'ADDSQ 'APPLY1 'DENR 'DEPENDSL
       'DFN_PROP 'DIFFSQ 'DOMAINP 'EQCAR 'ERROR1 'ERRORP 'ERRORSET* 'EXPTSQ
       'KERNP 'LASTPAIR 'LC 'LET 'LPOW 'MK*SQ 'MKQUOTE 'MKRN 'MKSQ 'MULTSQ
       'MVAR 'NCONC* 'NEQ 'NLIST 'NTH 'NUMR 'OPERATOR 'PREPSQ 'QUOTSQ 'RED
       'RULE-LIST 'SETCAR 'SFP 'SIMP* 'SIMPEXPT1 'SMEMQLP 'SUBTRSQ '*TAY2Q
       'CST-TAYLOR* 'HAS-TAYLOR* 'MAKE-CST-COEFFICIENT 'MAKE-TAYLOR*
       'PREPTAYEXP 'PRUNE-COEFFLIST 'SET-TAYORIG 'TAYCFPL 'TAYCFSQ
       'TAYCOEFFLIST 'TAYFLAGS 'TAYLOR*P 'TAYLOR-KERNEL-SQ-P 'TAYLOR-TRACE
       'TAYLOR-TRACE-MPRINT 'TAYLOR-TRACE-WITH-LEVEL '|TAYLOR:| 'TAYMAKECOEFF
       'TAYORIG 'TAYTEMPLATE 'TAYTPELNEXT 'TAYTPELORDER 'TAYTPELPOINT
       'TAYTPELVARS 'TAYTPVARS 'TAYVARS 'ADDTAYLOR* 'MULTTAYLOR 'MULTTAYLOR*
       'QUOTTAYLOR-AS-SQ 'PREPTAYLOR* 'INTTAYLORWRTTAYVAR 'TAYCOEFFLIST-UNION
       'TAYLOR1 'NZEROLIST 'REPLACE-NTH 'SMEMBERLP 'TAYLOR-ERROR 'TAYLOR-ERROR*
       'VAR-IS-NTH 'EXPTTAYRAT 'TAYSIMPSQ 'TAYSIMPSQ* 'ADD.COMP.TP.
       'ADDTO-ALL-TAYTPELORDERS 'ENTER-SORTED 'MULT.COMP.TP. 'SUBTR-TP-ORDER
       'TAYMINPOWERLIST 'TAYTP-MIN2 'TP-GREATERP 'TRUNCATE-TAYLOR*)) 
(FLUID
 '(*BACKTRACE *TAYLOR-ASSOC-LIST* *TAYEXPANDING* *TAYLORKEEPORIGINAL
   *TAYLORNOCACHE *TAYRESTART* *TRTAYLOR)) 
(GLOBAL '(*SQVAR* ERFG*)) 
(PUT '*TAY2Q* 'NUMBER-OF-ARGS 1) 
(PUT '*TAY2Q* 'DEFINED-ON-LINE '91) 
(PUT '*TAY2Q* 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT '*TAY2Q* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC '*TAY2Q* 'SMACRO '(LAMBDA (U) (CONS (CONS (CONS (CONS U 1) 1) NIL) 1))) 
(SWITCH (LIST 'TAYLORNOCACHE)) 
(PUT 'INIT-TAYLOR-CACHE 'NUMBER-OF-ARGS 0) 
(PUT 'INIT-TAYLOR-CACHE 'DEFINED-ON-LINE '97) 
(PUT 'INIT-TAYLOR-CACHE 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'INIT-TAYLOR-CACHE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INIT-TAYLOR-CACHE NIL (SETQ *TAYLOR-ASSOC-LIST* (CONS NIL *SQVAR*))) 
(PUT 'TAYLORNOCACHE 'SIMPFG '((T (INIT-TAYLOR-CACHE)))) 
(INIT-TAYLOR-CACHE) 
(PUT 'TAYLOR-ADD-TO-CACHE 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOR-ADD-TO-CACHE 'DEFINED-ON-LINE '104) 
(PUT 'TAYLOR-ADD-TO-CACHE 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOR-ADD-TO-CACHE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOR-ADD-TO-CACHE (KRNL TP RESULT)
    (COND
     ((NULL *TAYLORNOCACHE)
      (SETCAR *TAYLOR-ASSOC-LIST*
              (CONS (CONS (LIST KRNL (SUBLIS (LIST (CONS NIL NIL)) TP)) RESULT)
                    (CAR *TAYLOR-ASSOC-LIST*)))))) 
(FLUID '(*TAYLOR-EXPANSION-LEVEL* *TAYLOR-MAX-PRECISION-CYCLES*)) 
(SETQ *TAYLOR-EXPANSION-LEVEL* 0) 
(SETQ *TAYLOR-MAX-PRECISION-CYCLES* 6) 
(PUT 'TAYLOREXPAND 'NUMBER-OF-ARGS 2) 
(PUT 'TAYLOREXPAND 'DEFINED-ON-LINE '116) 
(PUT 'TAYLOREXPAND 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND (SQ TP)
    ((LAMBDA (*TAYLOR-EXPANSION-LEVEL*)
       (PROG (RESULT OLDKLIST *TAYEXPANDING* *TAYRESTART* LL CYCLES)
         (SETQ CYCLES 0)
         (SETQ LL TP)
         (SETQ OLDKLIST (GET 'TAYLOR* 'KLIST))
         (SETQ *TAYEXPANDING* T)
        RESTART
         (SETQ *TAYRESTART* NIL)
         (SETQ RESULT
                 (ERRORSET* (LIST 'TAYLOREXPAND1 (MKQUOTE SQ) (MKQUOTE LL) 'T)
                            *TRTAYLOR))
         (PUT 'TAYLOR* 'KLIST OLDKLIST)
         (COND
          ((NULL (ERRORP RESULT))
           (PROGN
            (SETQ RESULT (CAR RESULT))
            (COND
             ((AND (GREATERP CYCLES 0)
                   (AND (KERNP RESULT) (EQCAR (CAAAR (CAR RESULT)) 'TAYLOR*)))
              (SETQ RESULT
                      (CONS
                       (LIST
                        (CONS
                         (GETPOWER
                          (FKERN (TRUNCATE-TAYLOR* (CAAAR (CAR RESULT)) TP)) 1)
                         1))
                       1))))
            (RETURN RESULT))))
         (COND ((NULL *TAYRESTART*) (ERROR1)))
         (SETQ ERFG* NIL)
         ((LAMBDA (U)
            (COND
             (*TRTAYLOR
              (PROGN
               (LPRI
                (CONS "Taylor trace:"
                      (COND ((AND U (ATOM U)) (LIST U)) (T U))))
               (TERPRI)))))
          (LIST "Failed with template" LL))
         (SETQ CYCLES (PLUS CYCLES 1))
         (COND
          ((GREATERP CYCLES *TAYLOR-MAX-PRECISION-CYCLES*)
           (TAYLOR-ERROR 'MAX_CYCLES (DIFFERENCE CYCLES 1))))
         (SETQ LL (ADDTO-ALL-TAYTPELORDERS LL (NLIST 2 (LENGTH LL))))
         ((LAMBDA (U)
            (COND
             (*TRTAYLOR
              (PROGN
               (LPRI
                (CONS "Taylor trace:"
                      (COND ((AND U (ATOM U)) (LIST U)) (T U))))
               (TERPRI)))))
          (LIST "Restarting with template" LL))
         (GO RESTART)))
     (PLUS *TAYLOR-EXPANSION-LEVEL* 1))) 
(PUT 'TAYLOREXPAND1 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOREXPAND1 'DEFINED-ON-LINE '147) 
(PUT 'TAYLOREXPAND1 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND1 (SQ LL FLG)
    (PROG (OLDRESULT RESULT LLL LMIN DORESTART NL COUNT)
      (SETQ COUNT 0)
      (SETQ LLL LL)
      (COND
       ((NULL (CADR *TAYLOR-ASSOC-LIST*))
        (SETQ *TAYLOR-ASSOC-LIST* (CONS NIL *SQVAR*))))
     RESTART
      (SETQ COUNT (PLUS COUNT 1))
      (COND
       ((OR (GREATERP COUNT *TAYLOR-MAX-PRECISION-CYCLES*)
            (AND OLDRESULT (EQUAL (CADDR OLDRESULT) (CADDR RESULT))))
        (TAYLOR-ERROR 'MAX_CYCLES (DIFFERENCE COUNT 1))))
      (SETQ OLDRESULT RESULT)
      ((LAMBDA (G134)
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
                    (COND ((AND G134 (ATOM G134)) (LIST G134)) (T G134)))))
            (TERPRI)))))
       (LIST "expanding s.q. with template"
             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
               (SETQ EL LL)
               (COND ((NULL EL) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL)
                                   (LIST (CAR EL) (CADR EL)
                                         (PREPTAYEXP (CADDR EL))
                                         (PREPTAYEXP (CADDDR EL))))
                                 (CAR EL))
                                NIL)))
              LOOPLABEL
               (SETQ EL (CDR EL))
               (COND ((NULL EL) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (EL)
                           (LIST (CAR EL) (CADR EL) (PREPTAYEXP (CADDR EL))
                                 (PREPTAYEXP (CADDDR EL))))
                         (CAR EL))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
      (COND (*TRTAYLOR (MATHPRINT (MK*SQ SQ))))
      (COND
       ((EQUAL (CDR SQ) 1)
        (SETQ RESULT (TAYSIMPSQ* (TAYLOREXPAND-SF (CAR SQ) LLL T))))
       (T
        (PROG (NN DD)
          (SETQ DD (TAYLOREXPAND-SF (CDR SQ) LLL NIL))
          (COND ((NULL (CAR DD)) (TAYLOR-ERROR* 'ZERO-DENOM 'TAYLOREXPAND))
                ((NOT (AND (KERNP DD) (EQCAR (CAAAR (CAR DD)) 'TAYLOR*)))
                 (RETURN
                  (SETQ RESULT
                          (TAYSIMPSQ*
                           (MULTSQ (TAYLOREXPAND-SF (CAR SQ) LLL T)
                                   (INVSQ DD)))))))
          (SETQ LMIN
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
                   (CADR (CAAAR (CAR DD)))))
          (COND ((NULL LMIN) (TAYLOR-ERROR* 'ZERO-DENOM 'TAYLOREXPAND)))
          (SETQ LMIN (TAYMINPOWERLIST LMIN))
          (SETQ NN
                  (TAYLOREXPAND-SF (CAR SQ) (ADDTO-ALL-TAYTPELORDERS LLL LMIN)
                   T))
          (COND
           ((NOT
             (AND (AND (KERNP NN) (EQCAR (CAAAR (CAR NN)) 'TAYLOR*))
                  (AND (KERNP DD) (EQCAR (CAAAR (CAR DD)) 'TAYLOR*))))
            (SETQ RESULT (TAYSIMPSQ* (MULTSQ NN (INVSQ DD)))))
           (T (SETQ RESULT (QUOTTAYLOR-AS-SQ NN DD)))))))
      (COND
       ((NOT (AND (KERNP RESULT) (EQCAR (CAAAR (CAR RESULT)) 'TAYLOR*)))
        (RETURN
         (COND
          ((NOT
            (SMEMBERLP
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X LL)
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
             RESULT))
           (CONS
            (LIST
             (CONS
              (GETPOWER
               (FKERN
                (LIST 'TAYLOR*
                      (LIST
                       (CONS
                        (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                          (SETQ EL LL)
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
                      LL RESULT NIL))
               1)
              1))
            1))
          (T RESULT)))))
      (SETQ RESULT (CAAAR (CAR RESULT)))
      (SETQ DORESTART NIL)
      (PROG (LL1)
        (SETQ LL1 (CADDR RESULT))
        (PROG (I)
          (SETQ I (TAYEXP-DIFFERENCE (LENGTH LL1) (LENGTH LL)))
         LAB
          (COND
           ((TAYEXP-MINUSP
             (TAYEXP-TIMES (TAYEXP-MINUS 1) (TAYEXP-DIFFERENCE 1 I)))
            (RETURN NIL)))
          (SETQ LL (CONS (NTH LL1 I) LL))
          (SETQ I (TAYEXP-PLUS2 I (TAYEXP-MINUS 1)))
          (GO LAB))
        (COND
         (FLG
          (PROGN
           (SETQ NL (SUBTR-TP-ORDER LL LL1))
           (PROG (O)
             (SETQ O NL)
            LAB
             (COND ((NULL O) (RETURN NIL)))
             ((LAMBDA (O) (COND ((TAYEXP-GREATERP O 0) (SETQ DORESTART T))))
              (CAR O))
             (SETQ O (CDR O))
             (GO LAB))))))
      (COND
       (DORESTART
        (PROGN
         (SETQ LLL (ADDTO-ALL-TAYTPELORDERS LLL NL))
         ((LAMBDA (U)
            (COND
             (*TRTAYLOR
              (PROGN
               (LPRI
                (CONS "Taylor trace:"
                      (COND ((AND U (ATOM U)) (LIST U)) (T U))))
               (TERPRI)))))
          (LIST "restarting (loss of precision):" "old =" LL "result =" RESULT
                "new =" LLL))
         (GO RESTART))))
      (SETQ RESULT (TRUNCATE-TAYLOR* RESULT LL))
      (COND (*TAYLORKEEPORIGINAL (RPLACA (CDDDR RESULT) SQ)))
      (SETQ RESULT (CONS (LIST (CONS (GETPOWER (FKERN RESULT) 1) 1)) 1))
      ((LAMBDA (G136)
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
                    (COND ((AND G136 (ATOM G136)) (LIST G136)) (T G136)))))
            (TERPRI)))))
       (LIST "result of expanding s.q. is"))
      (COND (*TRTAYLOR (MATHPRINT (MK*SQ RESULT))))
      (RETURN RESULT))) 
(PUT 'TAYLOREXPAND-SF 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOREXPAND-SF 'DEFINED-ON-LINE '259) 
(PUT 'TAYLOREXPAND-SF 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND-SF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND-SF (SF LL FLG)
    (PROG (LCOF LP NEXT REST X DORESTART LLL XCL NL TPS L COUNT)
      (SETQ L 0)
      (SETQ COUNT 0)
      (SETQ LLL LL)
     RESTART
      (SETQ COUNT (PLUS COUNT 1))
      (COND
       ((GREATERP COUNT *TAYLOR-MAX-PRECISION-CYCLES*)
        (TAYLOR-ERROR 'MAX_CYCLES (DIFFERENCE COUNT 1))))
      ((LAMBDA (G138)
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
                    (COND ((AND G138 (ATOM G138)) (LIST G138)) (T G138)))))
            (TERPRI)))))
       (LIST "expanding s.f. with template"
             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
               (SETQ EL LL)
               (COND ((NULL EL) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL)
                                   (LIST (CAR EL) (CADR EL)
                                         (PREPTAYEXP (CADDR EL))
                                         (PREPTAYEXP (CADDDR EL))))
                                 (CAR EL))
                                NIL)))
              LOOPLABEL
               (SETQ EL (CDR EL))
               (COND ((NULL EL) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (EL)
                           (LIST (CAR EL) (CADR EL) (PREPTAYEXP (CADDR EL))
                                 (PREPTAYEXP (CADDDR EL))))
                         (CAR EL))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
      (COND (*TRTAYLOR (MATHPRINT (MK*SQ (CONS SF 1)))))
      (SETQ X (CONS NIL 1))
      (SETQ REST SF)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL REST))) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (ATOM REST) (ATOM (CAR REST)))
           (PROGN
            (SETQ NEXT
                    (CONS
                     (CONS
                      (CONS
                       (CONS
                        ((LAMBDA (G139)
                           (LIST 'TAYLOR*
                                 (LIST
                                  (CONS
                                   (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ EL LLL)
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
                                   G139))
                                 LLL G139 NIL))
                         (CONS REST 1))
                        1)
                       1)
                      NIL)
                     1))
            (SETQ REST NIL)))
          (T
           (PROGN
            (SETQ LP (TAYLOREXPAND-SP (CAAR REST) LLL FLG))
            (COND ((EQUAL (CDAR REST) 1) (SETQ NEXT LP))
                  (T
                   (PROGN
                    (SETQ LCOF (TAYLOREXPAND-SF (CDAR REST) LLL FLG))
                    (COND
                     ((AND
                       (AND (KERNP LCOF) (EQCAR (CAAAR (CAR LCOF)) 'TAYLOR*))
                       (AND (KERNP LP) (EQCAR (CAAAR (CAR LP)) 'TAYLOR*))
                       (SETQ TPS
                               (MULT.COMP.TP. (CAAAR (CAR LCOF))
                                (CAAAR (CAR LP)) NIL)))
                      (SETQ NEXT
                              (CONS
                               (CONS
                                (CONS
                                 (CONS
                                  (MULTTAYLOR* (CAAAR (CAR LCOF))
                                   (CAAAR (CAR LP)) TPS)
                                  1)
                                 1)
                                NIL)
                               1)))
                     (T (SETQ NEXT (TAYSIMPSQ* (MULTSQ LCOF LP)))))
                    NIL)))
            (SETQ REST (CDR REST)))))
         (COND ((NULL (CAR X)) (SETQ X NEXT))
               ((AND (AND (KERNP X) (EQCAR (CAAAR (CAR X)) 'TAYLOR*))
                     (AND (KERNP NEXT) (EQCAR (CAAAR (CAR NEXT)) 'TAYLOR*))
                     (SETQ TPS
                             (ADD.COMP.TP. (CAAAR (CAR X))
                              (CAAAR (CAR NEXT)))))
                (SETQ X
                        (CONS
                         (CONS
                          (CONS
                           (CONS
                            (ADDTAYLOR* (CAAAR (CAR X)) (CAAAR (CAR NEXT))
                             (CAR TPS))
                            1)
                           1)
                          NIL)
                         1)))
               (T (SETQ X (TAYSIMPSQ* (ADDSQ X NEXT))))))
        (GO WHILELABEL))
      (COND
       ((NOT (AND (KERNP X) (EQCAR (CAAAR (CAR X)) 'TAYLOR*))) (RETURN X)))
      (COND
       ((NULL FLG)
        (PROGN
         (SETQ XCL
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
                  (CADR (CAAAR (CAR X)))))
         (COND
          ((NULL XCL)
           (PROGN
            (SETQ LLL (ADDTO-ALL-TAYTPELORDERS LLL (NLIST 2 (LENGTH LLL))))
            ((LAMBDA (U)
               (COND
                (*TRTAYLOR
                 (PROGN
                  (LPRI
                   (CONS "Taylor trace:"
                         (COND ((AND U (ATOM U)) (LIST U)) (T U))))
                  (TERPRI)))))
             (LIST "restarting (no coeffs)...(" COUNT ")"))
            (GO RESTART)))
          (T
           (PROGN
            (SETQ L (TAYMINPOWERLIST XCL))
            (SETQ DORESTART NIL)
            (SETQ NL
                    (PROG (I FORALL-RESULT FORALL-ENDPTR)
                      (SETQ I 1)
                      (COND
                       ((TAYEXP-MINUSP (TAYEXP-DIFFERENCE (LENGTH LLL) I))
                        (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       (COND
                                        ((TAYEXP-GREATERP (NTH L I) 0)
                                         (PROGN
                                          (SETQ DORESTART T)
                                          (TAYEXP-TIMES 2 (NTH L I))))
                                        (T 0))
                                       NIL)))
                     LOOPLABEL
                      (SETQ I (TAYEXP-PLUS2 I 1))
                      (COND
                       ((TAYEXP-MINUSP (TAYEXP-DIFFERENCE (LENGTH LLL) I))
                        (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               (COND
                                ((TAYEXP-GREATERP (NTH L I) 0)
                                 (PROGN
                                  (SETQ DORESTART T)
                                  (TAYEXP-TIMES 2 (NTH L I))))
                                (T 0))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND
             (DORESTART
              (PROGN
               (SETQ FLG T)
               (SETQ LLL (ADDTO-ALL-TAYTPELORDERS LLL NL))
               ((LAMBDA (U)
                  (COND
                   (*TRTAYLOR
                    (PROGN
                     (LPRI
                      (CONS "Taylor trace:"
                            (COND ((AND U (ATOM U)) (LIST U)) (T U))))
                     (TERPRI)))))
                (LIST "restarting (no cst trm)...(" COUNT "):" "result ="
                      (CAAAR (CAR X)) "new =" LLL))
               (GO RESTART))))))))))
      ((LAMBDA (G142)
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
                    (COND ((AND G142 (ATOM G142)) (LIST G142)) (T G142)))))
            (TERPRI)))))
       (LIST "result of expanding s.f. is"))
      (COND (*TRTAYLOR (MATHPRINT (MK*SQ X))))
      (RETURN X))) 
(PUT 'TAYLOREXPAND-SP 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOREXPAND-SP 'DEFINED-ON-LINE '329) 
(PUT 'TAYLOREXPAND-SP 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND-SP 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND-SP (SP LL FLG)
    (PROG (FN KRNL POW SK VARS)
      ((LAMBDA (G144)
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
                    (COND ((AND G144 (ATOM G144)) (LIST G144)) (T G144)))))
            (TERPRI)))))
       (LIST "expanding s.p. with template"
             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
               (SETQ EL LL)
               (COND ((NULL EL) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (EL)
                                   (LIST (CAR EL) (CADR EL)
                                         (PREPTAYEXP (CADDR EL))
                                         (PREPTAYEXP (CADDDR EL))))
                                 (CAR EL))
                                NIL)))
              LOOPLABEL
               (SETQ EL (CDR EL))
               (COND ((NULL EL) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (EL)
                           (LIST (CAR EL) (CADR EL) (PREPTAYEXP (CADDR EL))
                                 (PREPTAYEXP (CADDDR EL))))
                         (CAR EL))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))
      (COND (*TRTAYLOR (MATHPRINT (MK*SQ (CONS (LIST (CONS SP 1)) 1)))))
      (SETQ VARS
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X LL)
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
                (GO LOOPLABEL)))
      (SETQ KRNL (CAR SP))
      (SETQ POW (CDR SP))
      (COND
       ((AND (IDP KRNL) (MEMQ KRNL VARS))
        (PROGN
         (SETQ POW 1)
         (SETQ SK
                 (CONS
                  (CONS (CONS (CONS (MAKE-POW-TAYLOR* KRNL (CDR SP) LL) 1) 1)
                        NIL)
                  1))
         NIL))
       ((AND (EQCAR KRNL 'EXPT) (MEMQ (CADR KRNL) VARS)
             (EQCAR (CADDR KRNL) 'QUOTIENT) (FIXP (CADR (CADDR KRNL)))
             (FIXP (CADDR (CADDR KRNL))))
        (PROGN
         (SETQ POW
                 (MKRN (TAYEXP-TIMES (CDR SP) (CADR (CADDR KRNL)))
                       (CADDR (CADDR KRNL))))
         (SETQ SK
                 (CONS
                  (CONS (CONS (CONS (MAKE-POW-TAYLOR* (CADR KRNL) POW LL) 1) 1)
                        NIL)
                  1))
         (SETQ POW 1)))
       ((SFP KRNL) (SETQ SK (TAYLOREXPAND-SF KRNL LL FLG)))
       ((SETQ SK (ASSOC (LIST SP LL) (CAR *TAYLOR-ASSOC-LIST*)))
        (PROGN (SETQ POW 1) (SETQ SK (CDR SK))))
       ((AND (NOT (EQUAL POW 1))
             (SETQ SK
                     (ASSOC (LIST (CONS KRNL 1) LL)
                            (CAR *TAYLOR-ASSOC-LIST*))))
        (SETQ SK (CDR SK)))
       (T
        (PROGN
         (SETQ SK
                 (COND
                  ((IDP KRNL)
                   (COND ((DEPENDSL KRNL VARS) (TAYLOREXPAND-DIFF KRNL LL FLG))
                         (T
                          (CONS
                           (CONS
                            (CONS
                             (CONS
                              ((LAMBDA (G145)
                                 (LIST 'TAYLOR*
                                       (LIST
                                        (CONS
                                         (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                                           (SETQ EL LL)
                                           (COND ((NULL EL) (RETURN NIL)))
                                           (SETQ FORALL-RESULT
                                                   (SETQ FORALL-ENDPTR
                                                           (CONS
                                                            ((LAMBDA (EL)
                                                               (NLIST 0
                                                                      (LENGTH
                                                                       (CAR
                                                                        EL))))
                                                             (CAR EL))
                                                            NIL)))
                                          LOOPLABEL
                                           (SETQ EL (CDR EL))
                                           (COND
                                            ((NULL EL) (RETURN FORALL-RESULT)))
                                           (RPLACD FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (EL)
                                                       (NLIST 0
                                                              (LENGTH
                                                               (CAR EL))))
                                                     (CAR EL))
                                                    NIL))
                                           (SETQ FORALL-ENDPTR
                                                   (CDR FORALL-ENDPTR))
                                           (GO LOOPLABEL))
                                         G145))
                                       LL G145 NIL))
                               (SIMP* KRNL))
                              1)
                             1)
                            NIL)
                           1))))
                  ((EQCAR KRNL 'TAYLOR*)
                   (COND
                    ((SMEMBERLP VARS
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X (CADDR KRNL))
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
                        (GO LOOPLABEL)))
                     (TAYLOREXPAND-SAMEVAR KRNL LL FLG))
                    (T (TAYLOREXPAND-TAYLOR KRNL LL FLG))))
                  ((NOT (IDP (CAR KRNL))) (TAYLOREXPAND-DIFF KRNL LL FLG))
                  ((NULL (SETQ FN (GET (CAR KRNL) 'TAYLORSIMPFN)))
                   (TAYLOREXPAND-DIFF KRNL LL FLG))
                  (T
                   (PROG (RES ARGS FLG *TAYLORAUTOEXPAND)
                     (SETQ ARGS
                             (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                               (SETQ EL (CDR KRNL))
                               (COND ((NULL EL) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (EL)
                                                   (COND
                                                    ((NOT (DEPENDSL EL VARS))
                                                     EL)
                                                    (T
                                                     (PROGN
                                                      (SETQ FLG T)
                                                      (PREPSQ
                                                       (TAYLOREXPAND (SIMP* EL)
                                                        LL))))))
                                                 (CAR EL))
                                                NIL)))
                              LOOPLABEL
                               (SETQ EL (CDR EL))
                               (COND ((NULL EL) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (EL)
                                           (COND ((NOT (DEPENDSL EL VARS)) EL)
                                                 (T
                                                  (PROGN
                                                   (SETQ FLG T)
                                                   (PREPSQ
                                                    (TAYLOREXPAND (SIMP* EL)
                                                     LL))))))
                                         (CAR EL))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                     (COND
                      ((SMEMBER 'TAYLOR* ARGS)
                       (SETQ RES (APPLY1 FN (CONS (CAR KRNL) ARGS))))
                      ((NULL FLG)
                       (SETQ RES
                               (CONS
                                (CONS
                                 (CONS
                                  (CONS
                                   ((LAMBDA (G147)
                                      (LIST 'TAYLOR*
                                            (LIST
                                             (CONS
                                              (PROG (EL FORALL-RESULT
                                                     FORALL-ENDPTR)
                                                (SETQ EL LL)
                                                (COND ((NULL EL) (RETURN NIL)))
                                                (SETQ FORALL-RESULT
                                                        (SETQ FORALL-ENDPTR
                                                                (CONS
                                                                 ((LAMBDA (EL)
                                                                    (NLIST 0
                                                                           (LENGTH
                                                                            (CAR
                                                                             EL))))
                                                                  (CAR EL))
                                                                 NIL)))
                                               LOOPLABEL
                                                (SETQ EL (CDR EL))
                                                (COND
                                                 ((NULL EL)
                                                  (RETURN FORALL-RESULT)))
                                                (RPLACD FORALL-ENDPTR
                                                        (CONS
                                                         ((LAMBDA (EL)
                                                            (NLIST 0
                                                                   (LENGTH
                                                                    (CAR EL))))
                                                          (CAR EL))
                                                         NIL))
                                                (SETQ FORALL-ENDPTR
                                                        (CDR FORALL-ENDPTR))
                                                (GO LOOPLABEL))
                                              G147))
                                            LL G147 NIL))
                                    (MKSQ KRNL 1))
                                   1)
                                  1)
                                 NIL)
                                1)))
                      (T (SETQ RES (MKSQ KRNL 1))))
                     (RETURN RES)))))
         (COND
          ((AND (KERNP SK) (EQCAR (CAAAR (CAR SK)) 'TAYLOR*))
           (TAYLOR-ADD-TO-CACHE (CONS KRNL 1) (CADDR (CAAAR (CAR SK))) SK))))))
      (COND
       ((NOT (EQUAL POW 1))
        (PROGN
         (COND
          ((NOT (AND (KERNP SK) (EQCAR (CAAAR (CAR SK)) 'TAYLOR*)))
           (SETQ SK (TAYSIMPSQ (EXPTSQ SK POW))))
          (T
           (PROGN
            (SETQ SK (CAAAR (CAR SK)))
            (SETQ SK
                    (CONS
                     (CONS
                      (CONS
                       (CONS
                        (COND ((EQUAL POW 2) (MULTTAYLOR SK SK))
                              (T (EXPTTAYRAT SK (CONS POW 1))))
                        1)
                       1)
                      NIL)
                     1)))))
         (COND
          ((AND (KERNP SK) (EQCAR (CAAAR (CAR SK)) 'TAYLOR*))
           (TAYLOR-ADD-TO-CACHE SP (CADDR (CAAAR (CAR SK))) SK))))))
      ((LAMBDA (G150)
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
                    (COND ((AND G150 (ATOM G150)) (LIST G150)) (T G150)))))
            (TERPRI)))))
       (LIST "result of expanding s.p. is"))
      (COND (*TRTAYLOR (MATHPRINT (MK*SQ SK))))
      (RETURN SK))) 
(PUT 'MAKE-POW-TAYLOR* 'NUMBER-OF-ARGS 3) 
(PUT 'MAKE-POW-TAYLOR* 'DEFINED-ON-LINE '408) 
(PUT 'MAKE-POW-TAYLOR* 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'MAKE-POW-TAYLOR* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE-POW-TAYLOR* (KRNL POW LL)
    (PROG (POS VAR0 NXT ORDR X TORIG POS1)
      (SETQ POS1 0)
      (SETQ POS (VAR-IS-NTH LL KRNL))
      (SETQ POS1 (CDR POS))
      (SETQ POS (CAR POS))
      (SETQ VAR0 (CADR (NTH LL POS)))
      (SETQ ORDR (CADDR (NTH LL POS)))
      (SETQ NXT (CADDDR (NTH LL POS)))
      (COND
       (*TAYLORKEEPORIGINAL
        (SETQ TORIG
                (COND ((EQCAR POW '|:RN:|) (SIMPEXPT1 KRNL (CDR POW) T))
                      (T (MKSQ KRNL POW))))))
      (SETQ LL
              (REPLACE-NTH LL POS
               ((LAMBDA (TPEL)
                  (LIST (CAR TPEL) (CADR TPEL) (TAYEXP-MAX2 POW ORDR)
                        (TAYEXP-PLUS (TAYEXP-MAX2 POW ORDR)
                                     (TAYEXP-DIFFERENCE NXT ORDR))))
                (NTH LL POS))))
      (COND
       ((OR (EQUAL VAR0 0) (EQ VAR0 'INFINITY))
        (RETURN
         (LIST 'TAYLOR*
               (LIST (MAKE-VAR-COEFFLIST LL POS POS1 POW (EQ VAR0 'INFINITY)))
               LL TORIG NIL))))
      (SETQ X
              (LIST 'TAYLOR*
                    (LIST
                     (CONS
                      (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                        (SETQ EL LL)
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
                      (SIMP* VAR0))
                     (MAKE-VAR-COEFFLIST LL POS POS1 1 NIL))
                    LL NIL NIL))
      (COND
       ((NOT (EQUAL POW 1))
        (SETQ X
                (EXPTTAYRAT X
                 (COND ((EQCAR POW '|:RN:|) (CDR POW)) (T (CONS POW 1)))))))
      (COND (*TAYLORKEEPORIGINAL (RPLACA (CDDDR X) TORIG)))
      (RETURN X))) 
(PUT 'MAKE-VAR-COEFFLIST 'NUMBER-OF-ARGS 5) 
(PUT 'MAKE-VAR-COEFFLIST 'DEFINED-ON-LINE '457) 
(PUT 'MAKE-VAR-COEFFLIST 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'MAKE-VAR-COEFFLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE-VAR-COEFFLIST (TP POS POS1 POW INFFLG)
    (CONS (MAKE-VAR-POWERLIST TP POS POS1 POW INFFLG) (CONS 1 1))) 
(PUT 'MAKE-VAR-POWERLIST 'NUMBER-OF-ARGS 5) 
(PUT 'MAKE-VAR-POWERLIST 'DEFINED-ON-LINE '460) 
(PUT 'MAKE-VAR-POWERLIST 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'MAKE-VAR-POWERLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKE-VAR-POWERLIST (TP POS POS1 POW INFFLG)
    (COND ((NULL TP) NIL)
          (T
           (CONS
            ((LAMBDA (L)
               (COND
                ((EQUAL POS 1)
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J 1)
                   (COND ((MINUSP (DIFFERENCE L J)) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    (COND ((NEQ J POS1) 0) (INFFLG (MINUS POW))
                                          (T POW))
                                    NIL)))
                  LOOPLABEL
                   (SETQ J (PLUS2 J 1))
                   (COND ((MINUSP (DIFFERENCE L J)) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            (COND ((NEQ J POS1) 0) (INFFLG (MINUS POW))
                                  (T POW))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
                (T (NLIST 0 L))))
             (LENGTH (CAR (CAR TP))))
            (MAKE-VAR-POWERLIST (CDR TP) (DIFFERENCE POS 1) POS1 POW INFFLG))))) 
(PUT 'TAYLOREXPAND-TAYLOR 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOREXPAND-TAYLOR 'DEFINED-ON-LINE '472) 
(PUT 'TAYLOREXPAND-TAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND-TAYLOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND-TAYLOR (TKRNL LL FLG)
    (PROG (TAY NOTAY X)
      (SETQ NOTAY (CONS NIL 1))
      (PROG (CC)
        (SETQ CC (CADR TKRNL))
       LAB
        (COND ((NULL CC) (RETURN NIL)))
        ((LAMBDA (CC)
           (PROGN
            (SETQ X (TAYLOREXPAND1 (CDR CC) LL FLG))
            (COND
             ((AND (KERNP X) (EQCAR (CAAAR (CAR X)) 'TAYLOR*))
              (SETQ TAY
                      (NCONC TAY
                             (PROG (CC2 FORALL-RESULT FORALL-ENDPTR)
                               (SETQ CC2 (CADR (CAAAR (CAR X))))
                               (COND ((NULL CC2) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (CC2)
                                                   (CONS
                                                    (APPEND (CAR CC) (CAR CC2))
                                                    (CDR CC2)))
                                                 (CAR CC2))
                                                NIL)))
                              LOOPLABEL
                               (SETQ CC2 (CDR CC2))
                               (COND ((NULL CC2) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (CC2)
                                           (CONS (APPEND (CAR CC) (CAR CC2))
                                                 (CDR CC2)))
                                         (CAR CC2))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))))
             (T (TAYLOR-ERROR 'EXPANSION "(possbile singularity)")))))
         (CAR CC))
        (SETQ CC (CDR CC))
        (GO LAB))
      (RETURN
       (COND ((NULL TAY) (CONS NIL 1))
             (T
              (CONS
               (CONS
                (CONS
                 (CONS (LIST 'TAYLOR* TAY (APPEND (CADDR TKRNL) LL) NIL NIL) 1)
                 1)
                NIL)
               1)))))) 
(FLUID '(**TAYLOREXPAND-DIFF-CACHE**)) 
(PUT 'TAYLOREXPAND-DIFF 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOREXPAND-DIFF 'DEFINED-ON-LINE '502) 
(PUT 'TAYLOREXPAND-DIFF 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND-DIFF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND-DIFF (KRNL LL FLG)
    (PROG (RESULT)
      (COND
       ((AND (NULL (ATOM KRNL)) (GET (CAR KRNL) (DFN_PROP KRNL)))
        (PROGN
         ((LAMBDA (**TAYLOREXPAND-DIFF-CACHE**)
            (SETQ RESULT
                    (ERRORSET*
                     (LIST 'TAYLOREXPAND-DIFF1 (MKQUOTE KRNL) (MKQUOTE LL)
                           (MKQUOTE FLG))
                     *BACKTRACE)))
          **TAYLOREXPAND-DIFF-CACHE**)
         NIL)))
      (COND ((AND RESULT (NOT (ERRORP RESULT))) (SETQ RESULT (CAR RESULT)))
            (*TAYRESTART* (ERROR1))
            (T
             (PROGN
              (SETQ RESULT (CONS (LIST (CONS (CONS KRNL 1) 1)) 1))
              (PROG (EL)
                (SETQ EL LL)
               LAB
                (COND ((NULL EL) (RETURN NIL)))
                ((LAMBDA (EL)
                   (SETQ RESULT
                           (CONS
                            (CONS
                             (CONS
                              (CONS
                               (TAYLOR1 RESULT (CAR EL) (CADR EL) (CADDR EL))
                               1)
                              1)
                             NIL)
                            1)))
                 (CAR EL))
                (SETQ EL (CDR EL))
                (GO LAB)))))
      (COND
       ((AND *TAYLORKEEPORIGINAL
             (AND (KERNP RESULT) (EQCAR (CAAAR (CAR RESULT)) 'TAYLOR*)))
        (RPLACA (CDDDR (CAAAR (CAR RESULT)))
                (CONS (LIST (CONS (CONS KRNL 1) 1)) 1))))
      (RETURN RESULT))) 
(PUT 'TAYLOREXPAND-DIFF1 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOREXPAND-DIFF1 'DEFINED-ON-LINE '549) 
(PUT 'TAYLOREXPAND-DIFF1 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND-DIFF1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND-DIFF1 (KRNL LL FLG)
    ((LAMBDA (RESULT Y)
       (PROGN
        (COND
         ((AND Y
               (EQUAL
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X LL)
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
                (PROG (X FORALL-RESULT FORALL-ENDPTR)
                  (SETQ X (SETQ Y (CDR Y)))
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
                  (GO LOOPLABEL)))
               (NOT (TP-GREATERP Y LL)))
          (SETQ LL
                  (PROG (EL FORALL-RESULT FORALL-ENDPTR)
                    (SETQ EL Y)
                    (COND ((NULL EL) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (EL)
                                        (LIST (CAR EL) (CADR EL)
                                              (DIFFERENCE (CADDR EL) 1)
                                              (DIFFERENCE (CADDDR EL) 1)))
                                      (CAR EL))
                                     NIL)))
                   LOOPLABEL
                    (SETQ EL (CDR EL))
                    (COND ((NULL EL) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (EL)
                                (LIST (CAR EL) (CADR EL)
                                      (DIFFERENCE (CADDR EL) 1)
                                      (DIFFERENCE (CADDDR EL) 1)))
                              (CAR EL))
                             NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))))
        (SETQ **TAYLOREXPAND-DIFF-CACHE**
                (CONS (CONS KRNL LL) **TAYLOREXPAND-DIFF-CACHE**))
        (PROG (EL)
          (SETQ EL LL)
         LAB
          (COND ((NULL EL) (RETURN NIL)))
          ((LAMBDA (EL) (SETQ RESULT (TAYLOREXPAND-DIFF2 RESULT EL NIL)))
           (CAR EL))
          (SETQ EL (CDR EL))
          (GO LAB))
        RESULT))
     (CONS (LIST (CONS (CONS KRNL 1) 1)) 1)
     (ASSOC KRNL **TAYLOREXPAND-DIFF-CACHE**))) 
(PUT 'TAYLOREXPAND-DIFF2 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOREXPAND-DIFF2 'DEFINED-ON-LINE '562) 
(PUT 'TAYLOREXPAND-DIFF2 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND-DIFF2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND-DIFF2 (SQ EL FLG)
    (PROG (L SINGLIST C0 TAY L0 TP TCL STERM)
      (SETQ SINGLIST (CONS NIL 1))
      (SETQ TP (LIST EL))
      (SETQ STERM
              (SIMP*
               (LIST 'TAYLORSINGULARITY (CAAAR (CAR SQ)) (CONS 'LIST (CAR EL))
                     (CADR EL))))
      (COND
       ((AND (KERNP STERM) (EQCAR (CAAAR (CAR STERM)) 'TAYLORSINGULARITY))
        (SETQ STERM NIL))
       (T (SETQ SQ (ADDSQ SQ (NEGSQ STERM)))))
      (COND
       ((GREATERP (CADDR EL) 0)
        (PROGN
         (SETQ L
                 (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                   (SETQ VAR (CAR EL))
                   (COND ((NULL VAR) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (VAR)
                                       (PROG (R)
                                         (SETQ R
                                                 (TAYLOREXPAND1 (DIFFSQ SQ VAR)
                                                  (LIST
                                                   (LIST (CAR EL) (CADR EL)
                                                         (DIFFERENCE (CADDR EL)
                                                                     1)
                                                         (DIFFERENCE
                                                          (CADDDR EL) 1)))
                                                  FLG))
                                         (COND
                                          ((AND (KERNP R)
                                                (EQCAR (CAAAR (CAR R))
                                                       'TAYLOR*))
                                           (RETURN
                                            (INTTAYLORWRTTAYVAR (CAAAR (CAR R))
                                             VAR)))
                                          (T
                                           (TAYLOR-ERROR 'EXPANSION
                                            "(possible singularity)")))))
                                     (CAR VAR))
                                    NIL)))
                  LOOPLABEL
                   (SETQ VAR (CDR VAR))
                   (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (VAR)
                               (PROG (R)
                                 (SETQ R
                                         (TAYLOREXPAND1 (DIFFSQ SQ VAR)
                                          (LIST
                                           (LIST (CAR EL) (CADR EL)
                                                 (DIFFERENCE (CADDR EL) 1)
                                                 (DIFFERENCE (CADDDR EL) 1)))
                                          FLG))
                                 (COND
                                  ((AND (KERNP R)
                                        (EQCAR (CAAAR (CAR R)) 'TAYLOR*))
                                   (RETURN
                                    (INTTAYLORWRTTAYVAR (CAAAR (CAR R)) VAR)))
                                  (T
                                   (TAYLOR-ERROR 'EXPANSION
                                    "(possible singularity)")))))
                             (CAR VAR))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ TCL
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
                            (CONS ((LAMBDA (PP) (CADR (CDR PP))) (CAR PP))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))
         (PROG (PP)
           (SETQ PP L)
          LAB
           (COND ((NULL PP) (RETURN NIL)))
           ((LAMBDA (PP)
              (COND ((CAR PP) (SETQ SINGLIST (ADDSQ SINGLIST (CAR PP))))))
            (CAR PP))
           (SETQ PP (CDR PP))
           (GO LAB))
         (COND
          ((NOT (NULL (CAR SINGLIST)))
           (TAYLOR-ERROR 'EXPANSION "(possible singularity)"))))))
      (COND ((NOT (NULL STERM)) (SETQ C0 (CONS NIL 1)))
            (T
             (PROGN
              (SETQ C0
                      (ERRORSET*
                       (LIST 'SUBSQ (MKQUOTE SQ)
                             (MKQUOTE
                              (PROG (VAR FORALL-RESULT FORALL-ENDPTR)
                                (SETQ VAR (CAR EL))
                                (COND ((NULL VAR) (RETURN NIL)))
                                (SETQ FORALL-RESULT
                                        (SETQ FORALL-ENDPTR
                                                (CONS
                                                 ((LAMBDA (VAR)
                                                    (CONS VAR (CADR EL)))
                                                  (CAR VAR))
                                                 NIL)))
                               LOOPLABEL
                                (SETQ VAR (CDR VAR))
                                (COND ((NULL VAR) (RETURN FORALL-RESULT)))
                                (RPLACD FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (VAR) (CONS VAR (CADR EL)))
                                          (CAR VAR))
                                         NIL))
                                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                (GO LOOPLABEL))))
                       *TRTAYLOR))
              (COND
               ((ERRORP C0)
                (TAYLOR-ERROR 'ZERO-DENOM "computation of constant term"))
               (T (SETQ C0 (CAR C0)))))))
      (SETQ L0 (LIST (NLIST 0 (LENGTH (CAR EL)))))
      (COND ((NULL (CAR C0)) NIL)
            ((NOT (AND (KERNP C0) (EQCAR (CAAAR (CAR C0)) 'TAYLOR*)))
             (SETQ TCL (CONS (CONS L0 C0) TCL)))
            (T
             (PROGN
              (SETQ C0 (CAAAR (CAR C0)))
              (SETQ TP (NCONC* (CADDR C0) TP))
              (PROG (PP)
                (SETQ PP (CADR C0))
               LAB
                (COND ((NULL PP) (RETURN NIL)))
                ((LAMBDA (PP)
                   (SETQ TCL
                           (ENTER-SORTED (CONS (APPEND (CAR PP) L0) (CDR PP))
                            TCL)))
                 (CAR PP))
                (SETQ PP (CDR PP))
                (GO LAB)))))
      (COND
       ((NOT (NULL L))
        (PROG (PP)
          (SETQ PP L)
         LAB
          (COND ((NULL PP) (RETURN NIL)))
          ((LAMBDA (PP) (SETQ TP (TAYTP-MIN2 TP (CADDR (CDR PP))))) (CAR PP))
          (SETQ PP (CDR PP))
          (GO LAB))))
      (SETQ TAY
              (CONS
               (CONS
                (CONS
                 (CONS
                  (LIST 'TAYLOR* TCL TP (COND (*TAYLORKEEPORIGINAL SQ) (T NIL))
                        NIL)
                  1)
                 1)
                NIL)
               1))
      (COND ((NOT (NULL (CAR SINGLIST))) (SETQ TAY (ADDSQ SINGLIST TAY))))
      (COND ((NULL STERM) (RETURN TAY))
            (T (RETURN (TAYSIMPSQ* (ADDSQ (TAYLOREXPAND STERM TP) TAY))))))) 
(AEVAL (OPERATOR (LIST 'TAYLORSINGULARITY))) 
(COND ((NULL (GET 'PSI 'SIMPFN)) (AEVAL (OPERATOR (LIST 'PSI))))) 
(AEVAL
 (LET
  '((LIST
     (REPLACEBY (TAYLORSINGULARITY (DILOG (~ X)) (~ Y) (~ Y0))
      (WHEN
       (DIFFERENCE (QUOTIENT (EXPT PI 2) 6)
                   (TIMES (LOG X) (LOG (DIFFERENCE 1 X))))
       (EQUAL (SUB (FOREACH Y1 IN Y COLLECT (EQUAL Y1 Y0)) X) 0)))
     (REPLACEBY (TAYLORSINGULARITY (EI (~ X)) (~ Y) (~ Y0))
      (WHEN
       (DIFFERENCE (QUOTIENT (DIFFERENCE (LOG X) (LOG (QUOTIENT 1 X))) 2)
                   (PSI 1))
       (EQUAL (SUB (FOREACH Y1 IN Y COLLECT (EQUAL Y1 Y0)) X) 0))))))) 
(PUT 'TAYLOREXPAND-SAMEVAR 'NUMBER-OF-ARGS 3) 
(PUT 'TAYLOREXPAND-SAMEVAR 'DEFINED-ON-LINE '644) 
(PUT 'TAYLOREXPAND-SAMEVAR 'DEFINED-IN-FILE 'TAYLOR/TAYEXPND.RED) 
(PUT 'TAYLOREXPAND-SAMEVAR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE TAYLOREXPAND-SAMEVAR (U LL FLG)
    (PROG (TPL)
      (SETQ TPL (CADDR U))
      (PROG (TPEL)
        (SETQ TPEL LL)
       LAB
        (COND ((NULL TPEL) (RETURN NIL)))
        ((LAMBDA (TPEL)
           (PROG (TP VARLIS MDEG N POS)
             (SETQ POS 0)
             (SETQ VARLIS (CAR TPEL))
             (SETQ POS (CAR (VAR-IS-NTH TPL (CAR VARLIS))))
             (SETQ TP (NTH TPL POS))
             (COND
              ((AND (TAYEXP-GREATERP (LENGTH VARLIS) 1)
                    (NOT (EQUAL VARLIS (CAR TP))))
               (TAYLOR-ERROR 'NOT-IMPLEMENTED
                "(homogeneous expansion in TAYLORSAMEVAR)")))
             (SETQ N (CADDR TPEL))
             (COND
              ((NEQ (CADR TP) (CADR TPEL))
               (SETQ U
                       (TAYLOR1
                        (COND ((NOT (NULL (CADDDR U))) (CADDDR U))
                              (T (SIMP* (PREPTAYLOR* U))))
                        VARLIS (CADR TPEL) N))))
             (SETQ MDEG (CADDR TP))
             (COND ((EQUAL N MDEG) NIL)
                   ((TAYEXP-GREATERP N MDEG)
                    (COND ((NULL FLG) NIL)
                          (T
                           (TAYLOR-ERROR 'EXPANSION
                            "Cannot expand further... truncated."))))
                   (T
                    (SETQ U
                            (LIST 'TAYLOR*
                                  (PROG (CC FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ CC (CADR U))
                                   STARTOVER
                                    (COND ((NULL CC) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            ((LAMBDA (CC)
                                               (COND
                                                ((TAYEXP-GREATERP
                                                  (NTH (NTH (CAR CC) POS) 1) N)
                                                 NIL)
                                                (T (LIST CC))))
                                             (CAR CC)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-RESULT))
                                    (SETQ CC (CDR CC))
                                    (COND
                                     ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                   LOOPLABEL
                                    (COND ((NULL CC) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            ((LAMBDA (CC)
                                               (COND
                                                ((TAYEXP-GREATERP
                                                  (NTH (NTH (CAR CC) POS) 1) N)
                                                 NIL)
                                                (T (LIST CC))))
                                             (CAR CC)))
                                    (SETQ FORALL-ENDPTR
                                            (LASTPAIR FORALL-ENDPTR))
                                    (SETQ CC (CDR CC))
                                    (GO LOOPLABEL))
                                  (REPLACE-NTH TPL POS
                                   (LIST VARLIS (CADR TPEL) N
                                         (TAYEXP-PLUS N 1)))
                                  (CADDDR U) (CAR (CDDDDR U))))))))
         (CAR TPEL))
        (SETQ TPEL (CDR TPEL))
        (GO LAB))
      (RETURN (CONS (CONS (CONS (CONS U 1) 1) NIL) 1)))) 
(ENDMODULE) 