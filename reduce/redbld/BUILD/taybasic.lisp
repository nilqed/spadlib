(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TAYBASIC)) 
(EXPORTS
 (LIST 'ADDTAYLOR 'ADDTAYLOR1 'INVTAYLOR 'INVTAYLOR1 'MAKECOEFFPAIRS
       'MAKECOEFFS 'MAKECOEFFS0 'MULTTAYLOR 'MULTTAYLOR1 'MULTTAYLORSQ
       'NEGTAYLOR 'NEGTAYLOR1 'QUOTTAYLOR 'QUOTTAYLOR1)) 
(IMPORTS
 (LIST 'ADDSQ 'INVSQ 'LASTPAIR 'MVAR 'MULTSQ 'NEGSQ 'NEQ 'NTH 'NUMR 'OVER
       'QUOTSQ 'REVERSIP 'SUBTRSQ 'UNION '*TAY2Q 'COMMON-INCREMENT 'GET-DEGREE
       'INVERT-POWERLIST 'MAKE-TAYLOR* 'MULTINTOCOEFFLIST 'PRUNE-COEFFLIST
       'SMALLEST-INCREMENT 'SUBTR-DEGREES 'SUBS2COEFFLIST 'TAYCFPL 'TAYCFSQ
       'TAYCOEFFLIST 'TAYFLAGS 'TAYFLAGSCOMBINE 'TAYGETCOEFF
       'TAYLOR-KERNEL-SQ-P '|TAYLOR:| 'TAYMAKECOEFF 'TAYORIG 'TAYTEMPLATE
       'TAYTPELVARS 'TPDEGREELIST 'TPNEXTLIST 'CONFUSION 'TAYLOR-ERROR
       'TAYLOR-ERROR* 'ADD.COMP.TP. 'ADD-DEGREES 'ENTER-SORTED 'EXCEEDS-ORDER
       'INV.TP. 'MIN2-ORDER 'MULT.COMP.TP. 'REPLACE-NEXT 'TAYDEGREE-STRICT<=)) 
(FLUID '(*TAYLORKEEPORIGINAL)) 
(PUT 'MULTTAYLORSQ 'NUMBER-OF-ARGS 2) 
(PUT 'MULTTAYLORSQ 'DEFINED-ON-LINE '65) 
(PUT 'MULTTAYLORSQ 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MULTTAYLORSQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTTAYLORSQ (TAY SQ)
    (COND ((OR (NULL TAY) (NULL (CAR SQ))) NIL)
          (T
           (LIST 'TAYLOR*
                 (PROG (P FORALL-RESULT FORALL-ENDPTR)
                   (SETQ P (CADR TAY))
                   (COND ((NULL P) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (P)
                                       (CONS (CAR P)
                                             (RESIMP
                                              (SUBS2* (MULTSQ (CDR P) SQ)))))
                                     (CAR P))
                                    NIL)))
                  LOOPLABEL
                   (SETQ P (CDR P))
                   (COND ((NULL P) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (P)
                               (CONS (CAR P)
                                     (RESIMP (SUBS2* (MULTSQ (CDR P) SQ)))))
                             (CAR P))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))
                 (CADDR TAY)
                 (COND
                  ((AND *TAYLORKEEPORIGINAL (CADDDR TAY))
                   (MULTSQ SQ (CADDDR TAY)))
                  (T NIL))
                 (CAR (CDDDDR TAY)))))) 
(DE DEGREE-UNION (U V) (UNION U V)) 
(PUT 'DEGREE-UNION 'NUMBER-OF-ARGS 2) 
(PUT 'DEGREE-UNION 'DEFINED-ON-LINE '81) 
(PUT 'DEGREE-UNION 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'DEGREE-UNION 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(PUTC 'DEGREE-UNION 'INLINE '(LAMBDA (U V) (UNION U V))) 
(PUT 'ADDTAYLOR 'NUMBER-OF-ARGS 2) 
(PUT 'ADDTAYLOR 'DEFINED-ON-LINE '84) 
(PUT 'ADDTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'ADDTAYLOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDTAYLOR (U V)
    ((LAMBDA (TP)
       (COND ((NULL TP) (CONFUSION 'ADDTAYLOR)) (T (ADDTAYLOR* U V (CAR TP)))))
     (ADD.COMP.TP. U V))) 
(PUT 'ADDTAYLOR-AS-SQ 'NUMBER-OF-ARGS 2) 
(PUT 'ADDTAYLOR-AS-SQ 'DEFINED-ON-LINE '94) 
(PUT 'ADDTAYLOR-AS-SQ 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'ADDTAYLOR-AS-SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDTAYLOR-AS-SQ (U V)
    (PROG (TP)
      (RETURN
       (COND
        ((AND (AND (KERNP U) (EQCAR (CAAAR (CAR U)) 'TAYLOR*))
              (AND (KERNP V) (EQCAR (CAAAR (CAR V)) 'TAYLOR*))
              (SETQ TP (ADD.COMP.TP. (CAAAR (CAR U)) (CAAAR (CAR V)))))
         (CONS
          (LIST
           (CONS
            (GETPOWER
             (FKERN (ADDTAYLOR* (CAAAR (CAR U)) (CAAAR (CAR V)) (CAR TP))) 1)
            1))
          1))
        (T (ADDSQ U V)))))) 
(PUT 'ADDTAYLOR* 'NUMBER-OF-ARGS 3) 
(PUT 'ADDTAYLOR* 'DEFINED-ON-LINE '103) 
(PUT 'ADDTAYLOR* 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'ADDTAYLOR* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDTAYLOR* (U V TP)
    ((LAMBDA (Z)
       (LIST 'TAYLOR* (CDR Z) (REPLACE-NEXT TP (CAR Z))
             (COND
              ((AND *TAYLORKEEPORIGINAL (CADDDR U) (CADDDR V))
               (ADDSQ (CADDDR U) (CADDDR V)))
              (T NIL))
             NIL))
     (ADDTAYLOR1 TP (CADR U) (CADR V)))) 
(PUT 'ADDTAYLOR1 'NUMBER-OF-ARGS 3) 
(PUT 'ADDTAYLOR1 'DEFINED-ON-LINE '112) 
(PUT 'ADDTAYLOR1 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'ADDTAYLOR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDTAYLOR1 (TMPL L1 L2)
    (PROG (CFF CLIST TN TP)
      (SETQ TP
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TMPL)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADDR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CADDR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ TN
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TMPL)
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
                (GO LOOPLABEL)))
      (SETQ CLIST
              (PROG (CC FORALL-RESULT FORALL-ENDPTR)
                (SETQ CC L1)
               STARTOVER
                (COND ((NULL CC) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (CC)
                           (COND
                            ((NOT (NULL (CAR (CDR CC))))
                             (COND
                              ((NOT (EXCEEDS-ORDER TN (CAR CC)))
                               (LIST (CONS (CAR CC) (CDR CC))))
                              (T
                               (PROGN
                                (SETQ TN (MIN2-ORDER TN TP (CAR CC)))
                                NIL))))))
                         (CAR CC)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ CC (CDR CC))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL CC) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (CC)
                           (COND
                            ((NOT (NULL (CAR (CDR CC))))
                             (COND
                              ((NOT (EXCEEDS-ORDER TN (CAR CC)))
                               (LIST (CONS (CAR CC) (CDR CC))))
                              (T
                               (PROGN
                                (SETQ TN (MIN2-ORDER TN TP (CAR CC)))
                                NIL))))))
                         (CAR CC)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ CC (CDR CC))
                (GO LOOPLABEL)))
      (PROG (CC)
        (SETQ CC L2)
       LAB
        (COND ((NULL CC) (RETURN NIL)))
        ((LAMBDA (CC)
           (COND
            ((NOT (NULL (CAR (CDR CC))))
             (COND
              ((NOT (EXCEEDS-ORDER TN (CAR CC)))
               (PROGN
                (SETQ CFF (ASSOC (CAR CC) CLIST))
                (COND ((NULL CFF) (SETQ CLIST (ENTER-SORTED CC CLIST)))
                      (T (RPLACD CFF (ADDSQ (CDR CFF) (CDR CC)))))))
              (T (SETQ TN (MIN2-ORDER TN TP (CAR CC))))))))
         (CAR CC))
        (SETQ CC (CDR CC))
        (GO LAB))
      (RETURN
       (CONS TN
             (PROG (PP FORALL-RESULT FORALL-ENDPTR)
               (SETQ PP CLIST)
              STARTOVER
               (COND ((NULL PP) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (PP)
                          ((LAMBDA (SQ)
                             (COND
                              ((NOT (NULL (CAR SQ)))
                               (LIST (CONS (CAR PP) SQ)))))
                           (SUBS2* (CDR PP))))
                        (CAR PP)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ PP (CDR PP))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL PP) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (PP)
                          ((LAMBDA (SQ)
                             (COND
                              ((NOT (NULL (CAR SQ)))
                               (LIST (CONS (CAR PP) SQ)))))
                           (SUBS2* (CDR PP))))
                        (CAR PP)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ PP (CDR PP))
               (GO LOOPLABEL)))))) 
(PUT 'NEGTAYLOR 'NUMBER-OF-ARGS 1) 
(PUT 'NEGTAYLOR 'DEFINED-ON-LINE '139) 
(PUT 'NEGTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'NEGTAYLOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEGTAYLOR (U)
    (LIST 'TAYLOR* (NEGTAYLOR1 (CADR U)) (CADDR U)
          (COND ((AND *TAYLORKEEPORIGINAL (CADDDR U)) (NEGSQ (CADDDR U)))
                (T NIL))
          (CAR (CDDDDR U)))) 
(PUT 'NEGTAYLOR1 'NUMBER-OF-ARGS 1) 
(PUT 'NEGTAYLOR1 'DEFINED-ON-LINE '147) 
(PUT 'NEGTAYLOR1 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'NEGTAYLOR1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NEGTAYLOR1 (TCL)
    (PROG (CC FORALL-RESULT FORALL-ENDPTR)
      (SETQ CC TCL)
      (COND ((NULL CC) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (CC) (CONS (CAR CC) (NEGSQ (CDR CC))))
                        (CAR CC))
                       NIL)))
     LOOPLABEL
      (SETQ CC (CDR CC))
      (COND ((NULL CC) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS ((LAMBDA (CC) (CONS (CAR CC) (NEGSQ (CDR CC)))) (CAR CC))
                    NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'MULTTAYLOR 'NUMBER-OF-ARGS 2) 
(PUT 'MULTTAYLOR 'DEFINED-ON-LINE '151) 
(PUT 'MULTTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MULTTAYLOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTTAYLOR (U V)
    ((LAMBDA (TPS)
       (COND ((NULL TPS) (CONFUSION 'MULTTAYLOR)) (T (MULTTAYLOR* U V TPS))))
     (MULT.COMP.TP. U V NIL))) 
(PUT 'MULTTAYLOR-AS-SQ 'NUMBER-OF-ARGS 2) 
(PUT 'MULTTAYLOR-AS-SQ 'DEFINED-ON-LINE '160) 
(PUT 'MULTTAYLOR-AS-SQ 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MULTTAYLOR-AS-SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTTAYLOR-AS-SQ (U V)
    (PROG (TPS)
      (RETURN
       (COND
        ((AND (AND (KERNP U) (EQCAR (CAAAR (CAR U)) 'TAYLOR*))
              (AND (KERNP V) (EQCAR (CAAAR (CAR V)) 'TAYLOR*))
              (SETQ TPS (MULT.COMP.TP. (CAAAR (CAR U)) (CAAAR (CAR V)) NIL)))
         (CONS
          (LIST
           (CONS
            (GETPOWER (FKERN (MULTTAYLOR* (CAAAR (CAR U)) (CAAAR (CAR V)) TPS))
                      1)
            1))
          1))
        (T (MULTSQ U V)))))) 
(PUT 'MULTTAYLOR* 'NUMBER-OF-ARGS 3) 
(PUT 'MULTTAYLOR* 'DEFINED-ON-LINE '169) 
(PUT 'MULTTAYLOR* 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MULTTAYLOR* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTTAYLOR* (U V TPS)
    ((LAMBDA (Z)
       (LIST 'TAYLOR* (CDR Z) (REPLACE-NEXT (CAR TPS) (CAR Z))
             (COND
              ((AND *TAYLORKEEPORIGINAL (CADDDR U) (CADDDR V))
               (MULTSQ (CADDDR U) (CADDDR V)))
              (T NIL))
             NIL))
     (MULTTAYLOR1 (CAR TPS) (CADR U) (CADR V)))) 
(PUT 'MULTTAYLOR1 'NUMBER-OF-ARGS 3) 
(PUT 'MULTTAYLOR1 'DEFINED-ON-LINE '178) 
(PUT 'MULTTAYLOR1 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MULTTAYLOR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MULTTAYLOR1 (TMPL L1 L2)
    (PROG (CFF PL RLIST SQ TN TP)
      (SETQ TP
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TMPL)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADDR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CADDR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ TN
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TMPL)
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
                (GO LOOPLABEL)))
      (PROG (CF1)
        (SETQ CF1 L1)
       LAB
        (COND ((NULL CF1) (RETURN NIL)))
        ((LAMBDA (CF1)
           (PROG (CF2)
             (SETQ CF2 L2)
            LAB
             (COND ((NULL CF2) (RETURN NIL)))
             ((LAMBDA (CF2)
                (PROGN
                 (SETQ PL (ADD-DEGREES (CAR CF1) (CAR CF2)))
                 (COND
                  ((NOT (EXCEEDS-ORDER TN PL))
                   (PROGN
                    (SETQ SQ (MULTSQ (CDR CF1) (CDR CF2)))
                    (COND
                     ((NOT (NULL (CAR SQ)))
                      (PROGN
                       (SETQ CFF (ASSOC PL RLIST))
                       (COND
                        ((NULL CFF)
                         (SETQ RLIST (ENTER-SORTED (CONS PL SQ) RLIST)))
                        (T (RPLACD CFF (ADDSQ (CDR CFF) SQ)))))))))
                  (T (SETQ TN (MIN2-ORDER TN TP PL))))))
              (CAR CF2))
             (SETQ CF2 (CDR CF2))
             (GO LAB)))
         (CAR CF1))
        (SETQ CF1 (CDR CF1))
        (GO LAB))
      (RETURN
       (CONS TN
             (PROG (PP FORALL-RESULT FORALL-ENDPTR)
               (SETQ PP RLIST)
              STARTOVER
               (COND ((NULL PP) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (PP)
                          ((LAMBDA (SQ)
                             (COND
                              ((NOT (NULL (CAR SQ)))
                               (LIST (CONS (CAR PP) SQ)))))
                           (SUBS2* (CDR PP))))
                        (CAR PP)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ PP (CDR PP))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL PP) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (PP)
                          ((LAMBDA (SQ)
                             (COND
                              ((NOT (NULL (CAR SQ)))
                               (LIST (CONS (CAR PP) SQ)))))
                           (SUBS2* (CDR PP))))
                        (CAR PP)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ PP (CDR PP))
               (GO LOOPLABEL)))))) 
(PUT 'QUOTTAYLOR 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTTAYLOR 'DEFINED-ON-LINE '277) 
(PUT 'QUOTTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'QUOTTAYLOR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTTAYLOR (U V)
    ((LAMBDA (TPS)
       (COND ((NULL TPS) (CONFUSION 'QUOTTAYLOR)) (T (QUOTTAYLOR* U V TPS))))
     (MULT.COMP.TP. U V T))) 
(PUT 'QUOTTAYLOR-AS-SQ 'NUMBER-OF-ARGS 2) 
(PUT 'QUOTTAYLOR-AS-SQ 'DEFINED-ON-LINE '287) 
(PUT 'QUOTTAYLOR-AS-SQ 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'QUOTTAYLOR-AS-SQ 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUOTTAYLOR-AS-SQ (U V)
    (PROG (TPS)
      (RETURN
       (COND
        ((AND (AND (KERNP U) (EQCAR (CAAAR (CAR U)) 'TAYLOR*))
              (AND (KERNP V) (EQCAR (CAAAR (CAR V)) 'TAYLOR*))
              (SETQ TPS (MULT.COMP.TP. (CAAAR (CAR U)) (CAAAR (CAR V)) T)))
         (CONS
          (LIST
           (CONS
            (GETPOWER (FKERN (QUOTTAYLOR* (CAAAR (CAR U)) (CAAAR (CAR V)) TPS))
                      1)
            1))
          1))
        (T (MULTSQ U (INVSQ V))))))) 
(PUT 'QUOTTAYLOR* 'NUMBER-OF-ARGS 3) 
(PUT 'QUOTTAYLOR* 'DEFINED-ON-LINE '296) 
(PUT 'QUOTTAYLOR* 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'QUOTTAYLOR* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE QUOTTAYLOR* (U V TPS)
    ((LAMBDA (Z)
       (LIST 'TAYLOR* (CDR Z) (REPLACE-NEXT (CAR TPS) (CAR Z))
             (COND
              ((AND *TAYLORKEEPORIGINAL (CADDDR U) (CADDDR V))
               (MULTSQ (CADDDR U) (INVSQ (CADDDR V))))
              (T NIL))
             NIL))
     (QUOTTAYLOR1 (CAR TPS) (CADR U) (CADR V)))) 
(PUT 'QUOTTAYLOR1 'NUMBER-OF-ARGS 3) 
(PUT 'QUOTTAYLOR1 'DEFINED-ON-LINE '305) 
(PUT 'QUOTTAYLOR1 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'QUOTTAYLOR1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE QUOTTAYLOR1 (TAY LU LV)
    (PROG (CLIST IL LMINU LMINV AMINU AMINV CCMIN COEFFLIS TP TN)
      (SETQ TP
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TAY)
                (COND ((NULL X) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (X) (CADDR X)) (CAR X)) NIL)))
               LOOPLABEL
                (SETQ X (CDR X))
                (COND ((NULL X) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (X) (CADDR X)) (CAR X)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ TN
              (PROG (X FORALL-RESULT FORALL-ENDPTR)
                (SETQ X TAY)
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
                (GO LOOPLABEL)))
      (SETQ LU
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
               LU))
      (COND ((NULL LU) (RETURN (CONS TN NIL))))
      (SETQ LMINU (CAR (CAR LU)))
      (PROG (EL)
        (SETQ EL (CDR LU))
       LAB
        (COND ((NULL EL) (RETURN NIL)))
        ((LAMBDA (EL)
           (COND
            ((NOT (NULL (CAR (CDR EL))))
             (SETQ LMINU (TAYDEGREE-MIN2 LMINU (CAR EL))))))
         (CAR EL))
        (SETQ EL (CDR EL))
        (GO LAB))
      (SETQ AMINU
              (COND
               ((NEQ LMINU (CAR (CAR LU)))
                ((LAMBDA (CC) (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                 (ASSOC LMINU LU)))
               (T (CDR (CAR LU)))))
      (SETQ LV
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
               LV))
      (COND ((NULL LV) (TAYLOR-ERROR* 'NOT-A-UNIT 'QUOTTAYLOR)))
      (SETQ IL
              (COMMON-INCREMENT (SMALLEST-INCREMENT LU)
               (SMALLEST-INCREMENT LV)))
      (SETQ AMINV (CDR (CAR LV)))
      (SETQ LMINV (CAR (CAR LV)))
      (PROG (CF)
        (SETQ CF (CDR LV))
       LAB
        (COND ((NULL CF) (RETURN NIL)))
        ((LAMBDA (CF)
           (COND
            ((NOT (TAYDEGREE-STRICT<= LMINV (CAR CF)))
             (TAYLOR-ERROR 'NOT-A-UNIT 'QUOTTAYLOR))))
         (CAR CF))
        (SETQ CF (CDR CF))
        (GO LAB))
      (SETQ CCMIN (SUBTR-DEGREES LMINU LMINV))
      (SETQ CLIST (LIST (CONS CCMIN (MULTSQ AMINU (INVSQ AMINV)))))
      (SETQ COEFFLIS (MAKECOEFFS CCMIN TN IL))
      (COND ((NULL COEFFLIS) (RETURN (CONS TN CLIST))))
      (PROG (CC)
        (SETQ CC (CDR COEFFLIS))
       LAB
        (COND ((NULL CC) (RETURN NIL)))
        ((LAMBDA (CC)
           (PROG (SQ)
             (SETQ SQ
                     (ADDSQ
                      ((LAMBDA (CC)
                         (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                       (ASSOC (ADD-DEGREES CC LMINV) LU))
                      (NEGSQ (ADDCOEFFS CLIST LV CCMIN CC))))
             (COND ((EXCEEDS-ORDER TN CC) (SETQ TN (MIN2-ORDER TN TP CC)))
                   ((NOT (NULL (CAR SQ)))
                    (SETQ CLIST
                            (CONS (CONS CC (MULTSQ SQ (INVSQ AMINV)))
                                  CLIST))))))
         (CAR CC))
        (SETQ CC (CDR CC))
        (GO LAB))
      (RETURN
       (CONS TN
             (PROG (PP FORALL-RESULT FORALL-ENDPTR)
               (SETQ PP (REVERSIP CLIST))
              STARTOVER
               (COND ((NULL PP) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (PP)
                          ((LAMBDA (SQ)
                             (COND
                              ((NOT (NULL (CAR SQ)))
                               (LIST (CONS (CAR PP) SQ)))))
                           (SUBS2* (CDR PP))))
                        (CAR PP)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ PP (CDR PP))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL PP) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (PP)
                          ((LAMBDA (SQ)
                             (COND
                              ((NOT (NULL (CAR SQ)))
                               (LIST (CONS (CAR PP) SQ)))))
                           (SUBS2* (CDR PP))))
                        (CAR PP)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ PP (CDR PP))
               (GO LOOPLABEL)))))) 
(PUT 'TAYDEGREE-MIN2 'NUMBER-OF-ARGS 2) 
(PUT 'TAYDEGREE-MIN2 'DEFINED-ON-LINE '346) 
(PUT 'TAYDEGREE-MIN2 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'TAYDEGREE-MIN2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TAYDEGREE-MIN2 (U V)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I 1)
      (COND ((MINUSP (DIFFERENCE (LENGTH U) I)) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       (PROG (L1 L2)
                         (SETQ L1 (NTH U I))
                         (SETQ L2 (NTH V I))
                         (RETURN
                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                            (SETQ J 1)
                            (COND
                             ((MINUSP (DIFFERENCE (LENGTH L1) J))
                              (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             (TAYEXP-MIN2 (NTH L1 J)
                                                          (NTH L2 J))
                                             NIL)))
                           LOOPLABEL
                            (SETQ J (PLUS2 J 1))
                            (COND
                             ((MINUSP (DIFFERENCE (LENGTH L1) J))
                              (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS (TAYEXP-MIN2 (NTH L1 J) (NTH L2 J))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL))))
                       NIL)))
     LOOPLABEL
      (SETQ I (PLUS2 I 1))
      (COND ((MINUSP (DIFFERENCE (LENGTH U) I)) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               (PROG (L1 L2)
                 (SETQ L1 (NTH U I))
                 (SETQ L2 (NTH V I))
                 (RETURN
                  (PROG (J FORALL-RESULT FORALL-ENDPTR)
                    (SETQ J 1)
                    (COND ((MINUSP (DIFFERENCE (LENGTH L1) J)) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS (TAYEXP-MIN2 (NTH L1 J) (NTH L2 J))
                                          NIL)))
                   LOOPLABEL
                    (SETQ J (PLUS2 J 1))
                    (COND
                     ((MINUSP (DIFFERENCE (LENGTH L1) J))
                      (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS (TAYEXP-MIN2 (NTH L1 J) (NTH L2 J)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'MAKECOEFFSHOM 'NUMBER-OF-ARGS 3) 
(PUT 'MAKECOEFFSHOM 'DEFINED-ON-LINE '360) 
(PUT 'MAKECOEFFSHOM 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MAKECOEFFSHOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKECOEFFSHOM (CMIN LASTTERM INCR)
    (COND ((NULL CMIN) '(NIL))
          (T
           (PROG (I FORALL-RESULT FORALL-ENDPTR)
             (SETQ I 0)
            STARTOVER
             (COND
              ((TAYEXP-MINUSP
                (TAYEXP-TIMES INCR (TAYEXP-DIFFERENCE LASTTERM I)))
               (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (PROG (L FORALL-RESULT FORALL-ENDPTR)
                       (SETQ L
                               (MAKECOEFFSHOM (CDR CMIN)
                                (TAYEXP-DIFFERENCE LASTTERM I) INCR))
                       (COND ((NULL L) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (L)
                                           (CONS (TAYEXP-PLUS (CAR CMIN) I) L))
                                         (CAR L))
                                        NIL)))
                      LOOPLABEL
                       (SETQ L (CDR L))
                       (COND ((NULL L) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (L)
                                   (CONS (TAYEXP-PLUS (CAR CMIN) I) L))
                                 (CAR L))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ I (TAYEXP-PLUS2 I INCR))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND
              ((TAYEXP-MINUSP
                (TAYEXP-TIMES INCR (TAYEXP-DIFFERENCE LASTTERM I)))
               (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (PROG (L FORALL-RESULT FORALL-ENDPTR)
                       (SETQ L
                               (MAKECOEFFSHOM (CDR CMIN)
                                (TAYEXP-DIFFERENCE LASTTERM I) INCR))
                       (COND ((NULL L) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (L)
                                           (CONS (TAYEXP-PLUS (CAR CMIN) I) L))
                                         (CAR L))
                                        NIL)))
                      LOOPLABEL
                       (SETQ L (CDR L))
                       (COND ((NULL L) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (L)
                                   (CONS (TAYEXP-PLUS (CAR CMIN) I) L))
                                 (CAR L))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ I (TAYEXP-PLUS2 I INCR))
             (GO LOOPLABEL))))) 
(PUT 'MAKECOEFFSHOM0 'NUMBER-OF-ARGS 3) 
(PUT 'MAKECOEFFSHOM0 'DEFINED-ON-LINE '367) 
(PUT 'MAKECOEFFSHOM0 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MAKECOEFFSHOM0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKECOEFFSHOM0 (NVARS LASTTERM INCR)
    (COND ((EQUAL NVARS 0) '(NIL))
          (T
           (PROG (I FORALL-RESULT FORALL-ENDPTR)
             (SETQ I 0)
            STARTOVER
             (COND
              ((TAYEXP-MINUSP
                (TAYEXP-TIMES INCR (TAYEXP-DIFFERENCE LASTTERM I)))
               (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (PROG (L FORALL-RESULT FORALL-ENDPTR)
                       (SETQ L
                               (MAKECOEFFSHOM0 (TAYEXP-DIFFERENCE NVARS 1)
                                (TAYEXP-DIFFERENCE LASTTERM I) INCR))
                       (COND ((NULL L) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (L) (CONS I L)) (CAR L))
                                             NIL)))
                      LOOPLABEL
                       (SETQ L (CDR L))
                       (COND ((NULL L) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (L) (CONS I L)) (CAR L)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ I (TAYEXP-PLUS2 I INCR))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND
              ((TAYEXP-MINUSP
                (TAYEXP-TIMES INCR (TAYEXP-DIFFERENCE LASTTERM I)))
               (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (PROG (L FORALL-RESULT FORALL-ENDPTR)
                       (SETQ L
                               (MAKECOEFFSHOM0 (TAYEXP-DIFFERENCE NVARS 1)
                                (TAYEXP-DIFFERENCE LASTTERM I) INCR))
                       (COND ((NULL L) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS ((LAMBDA (L) (CONS I L)) (CAR L))
                                             NIL)))
                      LOOPLABEL
                       (SETQ L (CDR L))
                       (COND ((NULL L) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (L) (CONS I L)) (CAR L)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ I (TAYEXP-PLUS2 I INCR))
             (GO LOOPLABEL))))) 
(PUT 'MAKECOEFFS 'NUMBER-OF-ARGS 3) 
(PUT 'MAKECOEFFS 'DEFINED-ON-LINE '374) 
(PUT 'MAKECOEFFS 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MAKECOEFFS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKECOEFFS (PLMIN DGL IL)
    (COND ((NULL PLMIN) '(NIL))
          (T
           (PROG (L1 FORALL-RESULT FORALL-ENDPTR)
             (SETQ L1 (MAKECOEFFS (CDR PLMIN) (CDR DGL) (CDR IL)))
            STARTOVER
             (COND ((NULL L1) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (L1)
                        (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ L2
                                  (MAKECOEFFSHOM (CAR PLMIN)
                                   (TAYEXP-DIFFERENCE
                                    (TAYEXP-DIFFERENCE (CAR DGL)
                                                       (PROG (N FORALL-RESULT)
                                                         (SETQ N (CAR PLMIN))
                                                         (SETQ FORALL-RESULT 0)
                                                        LAB1
                                                         (COND
                                                          ((NULL N)
                                                           (RETURN
                                                            FORALL-RESULT)))
                                                         (SETQ FORALL-RESULT
                                                                 (TAYEXP-PLUS
                                                                  ((LAMBDA (N)
                                                                     N)
                                                                   (CAR N))
                                                                  FORALL-RESULT))
                                                         (SETQ N (CDR N))
                                                         (GO LAB1)))
                                    (CAR IL))
                                   (CAR IL)))
                          (COND ((NULL L2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (L2) (CONS L2 L1))
                                            (CAR L2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ L2 (CDR L2))
                          (COND ((NULL L2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (L2) (CONS L2 L1)) (CAR L2))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR L1)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ L1 (CDR L1))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL L1) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (L1)
                        (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ L2
                                  (MAKECOEFFSHOM (CAR PLMIN)
                                   (TAYEXP-DIFFERENCE
                                    (TAYEXP-DIFFERENCE (CAR DGL)
                                                       (PROG (N FORALL-RESULT)
                                                         (SETQ N (CAR PLMIN))
                                                         (SETQ FORALL-RESULT 0)
                                                        LAB1
                                                         (COND
                                                          ((NULL N)
                                                           (RETURN
                                                            FORALL-RESULT)))
                                                         (SETQ FORALL-RESULT
                                                                 (TAYEXP-PLUS
                                                                  ((LAMBDA (N)
                                                                     N)
                                                                   (CAR N))
                                                                  FORALL-RESULT))
                                                         (SETQ N (CDR N))
                                                         (GO LAB1)))
                                    (CAR IL))
                                   (CAR IL)))
                          (COND ((NULL L2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (L2) (CONS L2 L1))
                                            (CAR L2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ L2 (CDR L2))
                          (COND ((NULL L2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (L2) (CONS L2 L1)) (CAR L2))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR L1)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ L1 (CDR L1))
             (GO LOOPLABEL))))) 
(PUT 'MAKECOEFFS0 'NUMBER-OF-ARGS 3) 
(PUT 'MAKECOEFFS0 'DEFINED-ON-LINE '390) 
(PUT 'MAKECOEFFS0 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MAKECOEFFS0 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKECOEFFS0 (TP DGL IL)
    (COND ((NULL TP) '(NIL))
          (T
           (PROG (L1 FORALL-RESULT FORALL-ENDPTR)
             (SETQ L1 (MAKECOEFFS0 (CDR TP) (CDR DGL) (CDR IL)))
            STARTOVER
             (COND ((NULL L1) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (L1)
                        (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ L2
                                  (MAKECOEFFSHOM0 (LENGTH (CAR (CAR TP)))
                                   (TAYEXP-DIFFERENCE (CAR DGL) (CAR IL))
                                   (CAR IL)))
                          (COND ((NULL L2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (L2) (CONS L2 L1))
                                            (CAR L2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ L2 (CDR L2))
                          (COND ((NULL L2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (L2) (CONS L2 L1)) (CAR L2))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR L1)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ L1 (CDR L1))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL L1) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (L1)
                        (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ L2
                                  (MAKECOEFFSHOM0 (LENGTH (CAR (CAR TP)))
                                   (TAYEXP-DIFFERENCE (CAR DGL) (CAR IL))
                                   (CAR IL)))
                          (COND ((NULL L2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (L2) (CONS L2 L1))
                                            (CAR L2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ L2 (CDR L2))
                          (COND ((NULL L2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS ((LAMBDA (L2) (CONS L2 L1)) (CAR L2))
                                        NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR L1)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ L1 (CDR L1))
             (GO LOOPLABEL))))) 
(PUT 'MAKECOEFFPAIRS1 'NUMBER-OF-ARGS 4) 
(PUT 'MAKECOEFFPAIRS1 'DEFINED-ON-LINE '407) 
(PUT 'MAKECOEFFPAIRS1 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MAKECOEFFPAIRS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKECOEFFPAIRS1 (PLMIN PL LMIN IL)
    (COND ((NULL PL) '((NIL)))
          (T
           (PROG (L1 FORALL-RESULT FORALL-ENDPTR)
             (SETQ L1
                     (MAKECOEFFPAIRS1 (CDR PLMIN) (CDR PL) (CDR LMIN)
                      (CDR IL)))
            STARTOVER
             (COND ((NULL L1) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (L1)
                        (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ L2
                                  (MAKECOEFFPAIRSHOM (CAR PLMIN) (CAR PL)
                                   (CAR LMIN) (TAYEXP-MINUS (CAR IL))))
                          (COND ((NULL L2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (L2)
                                              (CONS (CONS (CAR L2) (CAR L1))
                                                    (CONS (CDR L2) (CDR L1))))
                                            (CAR L2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ L2 (CDR L2))
                          (COND ((NULL L2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (L2)
                                      (CONS (CONS (CAR L2) (CAR L1))
                                            (CONS (CDR L2) (CDR L1))))
                                    (CAR L2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR L1)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ L1 (CDR L1))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL L1) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (L1)
                        (PROG (L2 FORALL-RESULT FORALL-ENDPTR)
                          (SETQ L2
                                  (MAKECOEFFPAIRSHOM (CAR PLMIN) (CAR PL)
                                   (CAR LMIN) (TAYEXP-MINUS (CAR IL))))
                          (COND ((NULL L2) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  (SETQ FORALL-ENDPTR
                                          (CONS
                                           ((LAMBDA (L2)
                                              (CONS (CONS (CAR L2) (CAR L1))
                                                    (CONS (CDR L2) (CDR L1))))
                                            (CAR L2))
                                           NIL)))
                         LOOPLABEL
                          (SETQ L2 (CDR L2))
                          (COND ((NULL L2) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (L2)
                                      (CONS (CONS (CAR L2) (CAR L1))
                                            (CONS (CDR L2) (CDR L1))))
                                    (CAR L2))
                                   NIL))
                          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                          (GO LOOPLABEL)))
                      (CAR L1)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ L1 (CDR L1))
             (GO LOOPLABEL))))) 
(PUT 'MAKECOEFFPAIRS 'NUMBER-OF-ARGS 4) 
(PUT 'MAKECOEFFPAIRS 'DEFINED-ON-LINE '417) 
(PUT 'MAKECOEFFPAIRS 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MAKECOEFFPAIRS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKECOEFFPAIRS (PLMIN PL LMIN IL)
    (REVERSIP (CDR (MAKECOEFFPAIRS1 PLMIN PL LMIN IL)))) 
(PUT 'MAKECOEFFPAIRSHOM 'NUMBER-OF-ARGS 4) 
(PUT 'MAKECOEFFPAIRSHOM 'DEFINED-ON-LINE '420) 
(PUT 'MAKECOEFFPAIRSHOM 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'MAKECOEFFPAIRSHOM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAKECOEFFPAIRSHOM (CLOW CHIGH CLMIN INC)
    (COND ((NULL CLMIN) '((NIL)))
          (T
           (PROG (I FORALL-RESULT FORALL-ENDPTR)
             (SETQ I (CAR CHIGH))
            STARTOVER
             (COND
              ((TAYEXP-MINUSP
                (TAYEXP-TIMES INC (TAYEXP-DIFFERENCE (CAR CLOW) I)))
               (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (PROG (L FORALL-RESULT FORALL-ENDPTR)
                       (SETQ L
                               (MAKECOEFFPAIRSHOM (CDR CLOW) (CDR CHIGH)
                                (CDR CLMIN) INC))
                       (COND ((NULL L) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (L)
                                           (CONS (CONS I (CAR L))
                                                 (CONS
                                                  (TAYEXP-PLUS (CAR CHIGH)
                                                               (TAYEXP-DIFFERENCE
                                                                (CAR CLMIN) I))
                                                  (CDR L))))
                                         (CAR L))
                                        NIL)))
                      LOOPLABEL
                       (SETQ L (CDR L))
                       (COND ((NULL L) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (L)
                                   (CONS (CONS I (CAR L))
                                         (CONS
                                          (TAYEXP-PLUS (CAR CHIGH)
                                                       (TAYEXP-DIFFERENCE
                                                        (CAR CLMIN) I))
                                          (CDR L))))
                                 (CAR L))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ I (TAYEXP-PLUS2 I INC))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND
              ((TAYEXP-MINUSP
                (TAYEXP-TIMES INC (TAYEXP-DIFFERENCE (CAR CLOW) I)))
               (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (PROG (L FORALL-RESULT FORALL-ENDPTR)
                       (SETQ L
                               (MAKECOEFFPAIRSHOM (CDR CLOW) (CDR CHIGH)
                                (CDR CLMIN) INC))
                       (COND ((NULL L) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (L)
                                           (CONS (CONS I (CAR L))
                                                 (CONS
                                                  (TAYEXP-PLUS (CAR CHIGH)
                                                               (TAYEXP-DIFFERENCE
                                                                (CAR CLMIN) I))
                                                  (CDR L))))
                                         (CAR L))
                                        NIL)))
                      LOOPLABEL
                       (SETQ L (CDR L))
                       (COND ((NULL L) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (L)
                                   (CONS (CONS I (CAR L))
                                         (CONS
                                          (TAYEXP-PLUS (CAR CHIGH)
                                                       (TAYEXP-DIFFERENCE
                                                        (CAR CLMIN) I))
                                          (CDR L))))
                                 (CAR L))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ I (TAYEXP-PLUS2 I INC))
             (GO LOOPLABEL))))) 
(PUT 'ADDCOEFFS 'NUMBER-OF-ARGS 4) 
(PUT 'ADDCOEFFS 'DEFINED-ON-LINE '427) 
(PUT 'ADDCOEFFS 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'ADDCOEFFS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE ADDCOEFFS (CL1 CL2 PLLOW PLHIGH)
    (PROG (S IL)
      (SETQ S (CONS NIL 1))
      (SETQ IL
              (COMMON-INCREMENT (SMALLEST-INCREMENT CL1)
               (SMALLEST-INCREMENT CL2)))
      (PROG (P)
        (SETQ P (MAKECOEFFPAIRS PLLOW PLHIGH (CAAR CL2) IL))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (SETQ S
                   (ADDSQ S
                          (MULTSQ
                           ((LAMBDA (CC)
                              (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                            (ASSOC (CAR P) CL1))
                           ((LAMBDA (CC)
                              (COND ((NULL CC) (CONS NIL 1)) (T (CDR CC))))
                            (ASSOC (CDR P) CL2))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN S))) 
(PUT 'INVTAYLOR 'NUMBER-OF-ARGS 1) 
(PUT 'INVTAYLOR 'DEFINED-ON-LINE '440) 
(PUT 'INVTAYLOR 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'INVTAYLOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INVTAYLOR (U)
    (COND ((NULL U) (CONFUSION 'INVTAYLOR))
          (T
           (PROG (TPS)
             (SETQ TPS (INV.TP. U))
             (RETURN
              (LIST 'TAYLOR* (INVTAYLOR1 (CAR TPS) (CADR U)) (CAR TPS)
                    (COND
                     ((AND *TAYLORKEEPORIGINAL (CADDDR U)) (INVSQ (CADDDR U)))
                     (T NIL))
                    (CAR (CDDDDR U)))))))) 
(PUT 'INVTAYLOR1 'NUMBER-OF-ARGS 2) 
(PUT 'INVTAYLOR1 'DEFINED-ON-LINE '458) 
(PUT 'INVTAYLOR1 'DEFINED-IN-FILE 'TAYLOR/TAYBASIC.RED) 
(PUT 'INVTAYLOR1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INVTAYLOR1 (TAY L)
    (PROG (CLIST AMIN CCMIN COEFFLIS IL)
      (SETQ L
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
               L))
      (COND ((NULL L) (TAYLOR-ERROR* 'NOT-A-UNIT 'INVTAYLOR)))
      (SETQ AMIN (CDR (CAR L)))
      (SETQ CCMIN (CAR (CAR L)))
      (PROG (CF)
        (SETQ CF (CDR L))
       LAB
        (COND ((NULL CF) (RETURN NIL)))
        ((LAMBDA (CF)
           (COND
            ((NOT (TAYDEGREE-STRICT<= CCMIN (CAR CF)))
             (TAYLOR-ERROR 'NOT-A-UNIT 'INVTAYLOR))))
         (CAR CF))
        (SETQ CF (CDR CF))
        (GO LAB))
      (SETQ IL (SMALLEST-INCREMENT L))
      (SETQ CCMIN
              (PROG (NL FORALL-RESULT FORALL-ENDPTR)
                (SETQ NL CCMIN)
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
                                      (COND ((NULL P) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (P) (TAYEXP-MINUS P))
                                                (CAR P))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
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
                                               ((LAMBDA (P) (TAYEXP-MINUS P))
                                                (CAR P))
                                               NIL)))
                             LOOPLABEL
                              (SETQ P (CDR P))
                              (COND ((NULL P) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (P) (TAYEXP-MINUS P)) (CAR P))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR NL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CLIST (LIST (CONS CCMIN (INVSQ AMIN))))
      (SETQ COEFFLIS
              (MAKECOEFFS CCMIN
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X TAY)
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
      (COND
       ((NOT (NULL COEFFLIS))
        (PROG (CC)
          (SETQ CC (CDR COEFFLIS))
         LAB
          (COND ((NULL CC) (RETURN NIL)))
          ((LAMBDA (CC)
             (PROG (SQ)
               (SETQ SQ (ADDCOEFFS CLIST L CCMIN CC))
               (COND
                ((NOT (NULL (CAR SQ)))
                 (SETQ CLIST
                         (CONS (CONS CC (NEGSQ (MULTSQ SQ (INVSQ AMIN))))
                               CLIST))))))
           (CAR CC))
          (SETQ CC (CDR CC))
          (GO LAB))))
      (RETURN
       (PROG (PP FORALL-RESULT FORALL-ENDPTR)
         (SETQ PP (REVERSIP CLIST))
        STARTOVER
         (COND ((NULL PP) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (PP)
                    ((LAMBDA (SQ)
                       (COND
                        ((NOT (NULL (CAR SQ))) (LIST (CONS (CAR PP) SQ)))))
                     (SUBS2* (CDR PP))))
                  (CAR PP)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ PP (CDR PP))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL PP) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (PP)
                    ((LAMBDA (SQ)
                       (COND
                        ((NOT (NULL (CAR SQ))) (LIST (CONS (CAR PP) SQ)))))
                     (SUBS2* (CDR PP))))
                  (CAR PP)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ PP (CDR PP))
         (GO LOOPLABEL))))) 
(ENDMODULE) 