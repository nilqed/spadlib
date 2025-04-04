(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'NONCOM2)) 
(CREATE-PACKAGE '(NONCOM2) '(CONTRIB PHYSICS)) 
(FLUID '(*NOSQ *MYMATCH FRLIS* NCMP* SUBFG* WTL*)) 
(SETQ *NOSQ T) 
(SWITCH (LIST 'MYMATCH)) 
(SETQ *MYMATCH T) 
(PUT 'TRWRITE 'NUMBER-OF-ARGS 1) 
(PUT 'TRWRITE 'DEFINED-ON-LINE '96) 
(PUT 'TRWRITE 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'TRWRITE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TRWRITE (U)
    (PROG (X)
      (COND ((NOT (FLAGP (CAR U) 'TRACING)) (RETURN NIL)))
      (PROGN (PRIN2 "**in procedure: ") (PRIN2 (CAR U)) NIL)
      (TERPRI)
      (PROG (X)
        (SETQ X (CDR U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (PROGN (PRIN2 X) NIL)) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (TERPRI))) 
(PUT 'FUNTRACE 'NUMBER-OF-ARGS 1) 
(PUT 'FUNTRACE 'DEFINED-ON-LINE '105) 
(PUT 'FUNTRACE 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'FUNTRACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FUNTRACE (U)
    (PROG (X)
      (SETQ X U)
     LAB
      (COND ((NULL X) (RETURN NIL)))
      ((LAMBDA (X) (FLAG (LIST X) 'TRACING)) (CAR X))
      (SETQ X (CDR X))
      (GO LAB))) 
(DEFLIST '((TRWRITE RLIS) (FUNTRACE RLIS)) 'STAT) 
(PUT 'PNTH* 'NUMBER-OF-ARGS 2) 
(PUT 'PNTH* 'DEFINED-ON-LINE '110) 
(PUT 'PNTH* 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'PNTH* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PNTH* (U N)
    (COND ((NULL U) NIL) ((EQUAL N 1) U) (T (PNTH* (CDR U) (DIFFERENCE N 1))))) 
(PUT 'NTH* 'NUMBER-OF-ARGS 2) 
(PUT 'NTH* 'DEFINED-ON-LINE '115) 
(PUT 'NTH* 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NTH* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NTH* (U N) (COND ((LESSP (LENGTH U) N) NIL) (T (CAR (PNTH* U N))))) 
(PUT 'REVASSOC 'NUMBER-OF-ARGS 2) 
(PUT 'REVASSOC 'DEFINED-ON-LINE '119) 
(PUT 'REVASSOC 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'REVASSOC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REVASSOC (U V)
    (PROG (X)
      (COND ((NOT (LISTP V)) (REDERR "invalid argument to revassoc")))
     A
      (COND ((NULL V) (RETURN NIL)))
      (SETQ X (CAR V))
      (SETQ V (CDR V))
      (COND ((AND (PAIRP X) (EQUAL (CDR X) U)) (RETURN (CAR X))))
      (GO A))) 
(PUT 'KERNELP 'NUMBER-OF-ARGS 1) 
(PUT 'KERNELP 'DEFINED-ON-LINE '132) 
(PUT 'KERNELP 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'KERNELP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE KERNELP (U)
    (COND ((OR (NULL U) (DOMAIN*P U)) NIL) ((IDP U) T)
          ((AND (LISTP U) (IDP (CAR U))
                (NOT
                 (MEMQ (CAR U)
                       '(*SQ SET SETQ PLUS MINUS DIFFERENCE TIMES QUOTIENT))))
           T)
          (T NIL))) 
(PUT 'SPP 'NUMBER-OF-ARGS 1) 
(PUT 'SPP 'DEFINED-ON-LINE '142) 
(PUT 'SPP 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'SPP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SPP (U) (AND (PAIRP U) (KERNELP (CAR U)))) 
(PUT 'STP 'NUMBER-OF-ARGS 1) 
(PUT 'STP 'DEFINED-ON-LINE '146) 
(PUT 'STP 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'STP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STP (U) (AND (PAIRP U) (SPP (CAR U)))) 
(PUT 'SFP2 'NUMBER-OF-ARGS 1) 
(PUT 'SFP2 'DEFINED-ON-LINE '150) 
(PUT 'SFP2 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'SFP2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SFP2 (U) (AND (PAIRP U) (STP (CAR U)))) 
(PUT 'TSTP 'NUMBER-OF-ARGS 1) 
(PUT 'TSTP 'DEFINED-ON-LINE '155) 
(PUT 'TSTP 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'TSTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TSTP (U) (AND (STP U) (NEQ (CAR (PREPF (LIST U))) 'PLUS))) 
(PUT '**A2F 'NUMBER-OF-ARGS 1) 
(PUT '**A2F 'DEFINED-ON-LINE '159) 
(PUT '**A2F 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT '**A2F 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE **A2F (U)
    (PROG (FLG RES)
      (SETQ FLG SUBFG*)
      (SETQ SUBFG* NIL)
      (SETQ RES (*Q2F (SIMP* U)))
      (SETQ SUBFG* FLG)
      (RETURN RES))) 
(PUT '**A2Q 'NUMBER-OF-ARGS 1) 
(PUT '**A2Q 'DEFINED-ON-LINE '170) 
(PUT '**A2Q 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT '**A2Q 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE **A2Q (U)
    (COND ((EQ (CAR U) 'QUOTIENT) (CONS (**A2F (CADR U)) (**A2F (CADDR U))))
          (T (CONS (**A2F U) 1)))) 
(PUT '*A2Q 'NUMBER-OF-ARGS 1) 
(PUT '*A2Q 'DEFINED-ON-LINE '175) 
(PUT '*A2Q 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT '*A2Q 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *A2Q (U)
    (COND
     ((AND (NOT (ATOM U)) (EQ (CAR U) 'QUOTIENT))
      (CONS (*Q2F (SIMP* (CADR U))) (*Q2F (SIMP* (CADDR U)))))
     (T (CONS (*Q2F (SIMP* U)) 1)))) 
(PUT 'ATSOC2 'NUMBER-OF-ARGS 2) 
(PUT 'ATSOC2 'DEFINED-ON-LINE '190) 
(PUT 'ATSOC2 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'ATSOC2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ATSOC2 (U V)
    (PROG ()
     A
      (COND ((NULL V) (RETURN NIL)) ((EQCAR (CAAR V) U) (RETURN (CAR V))))
      (SETQ V (CDR V))
      (GO A))) 
(PUT 'NONCOM2_SUBLIST 'NUMBER-OF-ARGS 2) 
(PUT 'NONCOM2_SUBLIST 'DEFINED-ON-LINE '200) 
(PUT 'NONCOM2_SUBLIST 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NONCOM2_SUBLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NONCOM2_SUBLIST (U V)
    (PROG (X Z Y W RESLIST N U1)
      (COND
       ((NOT (AND (LISTP U) (LISTP V)))
        (REDERR " invalid arguments to noncom2_sublist")))
      (COND
       ((OR (NULL U) (NULL V) (NOT (SETQ V (MEMBER (CAR U) V)))) (RETURN NIL)))
     A
      (COND ((NULL U) (RETURN (NCONC RESLIST (APPEND U1 V)))))
      (SETQ Z V)
      (SETQ X (CAR U))
      (SETQ U (CDR U))
      (COND ((NOT (SETQ V (MEMBER X Z))) (RETURN NIL)))
      (SETQ V (CDR V))
      (SETQ N (DIFFERENCE (DIFFERENCE (LENGTH Z) (LENGTH V)) 1))
      (SETQ Z
              (PROG (K FORALL-RESULT FORALL-ENDPTR)
                (SETQ K 1)
                (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (NTH Z K) NIL)))
               LOOPLABEL
                (SETQ K (PLUS2 K 1))
                (COND ((MINUSP (DIFFERENCE N K)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (NTH Z K) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (TRWRITE (LIST 'NONCOM2_SUBLIST "z= " Z " v= " V " x= " X))
     A0
      (COND ((NULL Z) (PROGN (SETQ U1 (NCONC U1 (LIST X))) (GO A))))
      (SETQ W (CAR Z))
      (SETQ Z (CDR Z))
      (COND ((NONCOMMUTING_SPLIST W U1) (GO A1))
            (T (SETQ RESLIST (NCONC RESLIST (LIST W)))))
      (GO A0)
     A1
      (SETQ Z (REVERSE (CONS W Z)))
      (COND ((NONCOMMUTINGSP (CAR Z) X) (RETURN NIL)))
      (SETQ V (CONS (CAR Z) V))
      (SETQ Z (REVERSE (CDR Z)))
      (GO A0))) 
(PUT 'DELETEALL 'NUMBER-OF-ARGS 2) 
(PUT 'DELETEALL 'DEFINED-ON-LINE '234) 
(PUT 'DELETEALL 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'DELETEALL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DELETEALL (X U)
    (PROG (Y RES)
     A
      (COND ((NULL U) (RETURN RES)))
      (SETQ Y (CAR U))
      (SETQ U (CDR U))
      (COND ((NOT (EQUAL Y X)) (SETQ RES (NCONC RES (LIST Y)))))
      (GO A))) 
(PUT 'DELETEMULT 'NUMBER-OF-ARGS 2) 
(PUT 'DELETEMULT 'DEFINED-ON-LINE '245) 
(PUT 'DELETEMULT 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'DELETEMULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DELETEMULT (X U)
    (PROG (Y N)
      (COND ((NULL (SETQ Y (CDR (MEMBER X U)))) (RETURN U)))
      (SETQ N (DIFFERENCE (LENGTH U) (LENGTH Y)))
      (RETURN
       (NCONC
        (PROG (K FORALL-RESULT FORALL-ENDPTR)
          (SETQ K 1)
          (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
          (SETQ FORALL-RESULT (SETQ FORALL-ENDPTR (CONS (NTH U K) NIL)))
         LOOPLABEL
          (SETQ K (PLUS2 K 1))
          (COND ((MINUSP (DIFFERENCE N K)) (RETURN FORALL-RESULT)))
          (RPLACD FORALL-ENDPTR (CONS (NTH U K) NIL))
          (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
          (GO LOOPLABEL))
        (DELETEALL X Y))))) 
(PUT 'DELETEMULT* 'NUMBER-OF-ARGS 1) 
(PUT 'DELETEMULT* 'DEFINED-ON-LINE '256) 
(PUT 'DELETEMULT* 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'DELETEMULT* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETEMULT* (U)
    (PROG (X)
      (COND ((NULL U) (RETURN U)))
      (SETQ X (LIST (CAR U)))
      (SETQ U (CDR U))
      (PROG (Y)
        (SETQ Y U)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y) (COND ((NOT (MEMBER Y X)) (NCONC X (LIST Y))))) (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN X))) 
(PUT 'LISTOFVARNAMES 'NUMBER-OF-ARGS 1) 
(PUT 'LISTOFVARNAMES 'DEFINED-ON-LINE '270) 
(PUT 'LISTOFVARNAMES 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'LISTOFVARNAMES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTOFVARNAMES (U)
    (COND ((NOT (LISTP U)) (REDERR "invalid argument to listofvarnames"))
          (T
           (PROG (X FORALL-RESULT FORALL-ENDPTR)
             (SETQ X U)
             (COND ((NULL X) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (X)
                                 (COND ((DOMAIN*P X) (CONS NIL 'FREE))
                                       ((ATOM X) (CONS NIL 'FREE))
                                       ((IDP (CAR X)) (CONS (CAR X) 'FREE))
                                       ((IDP (CAAR X)) (CONS (CAAR X) 'FREE))))
                               (CAR X))
                              NIL)))
            LOOPLABEL
             (SETQ X (CDR X))
             (COND ((NULL X) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS
                      ((LAMBDA (X)
                         (COND ((DOMAIN*P X) (CONS NIL 'FREE))
                               ((ATOM X) (CONS NIL 'FREE))
                               ((IDP (CAR X)) (CONS (CAR X) 'FREE))
                               ((IDP (CAAR X)) (CONS (CAAR X) 'FREE))))
                       (CAR X))
                      NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'REPLSUBLIST 'NUMBER-OF-ARGS 3) 
(PUT 'REPLSUBLIST 'DEFINED-ON-LINE '283) 
(PUT 'REPLSUBLIST 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'REPLSUBLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE REPLSUBLIST (U V W)
    (PROG (N X RES)
      (COND ((NOT (SETQ X (NONCOM2_SUBLIST V W))) (RETURN W)))
      (SETQ N (DIFFERENCE (LENGTH W) (LENGTH X)))
      (SETQ RES
              (COND ((ZEROP N) NIL)
                    (T
                     (PROG (K FORALL-RESULT FORALL-ENDPTR)
                       (SETQ K 1)
                       (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR (CONS (NTH W K) NIL)))
                      LOOPLABEL
                       (SETQ K (PLUS2 K 1))
                       (COND
                        ((MINUSP (DIFFERENCE N K)) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR (CONS (NTH W K) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ RES (COND ((NULL RES) U) (T (NCONC RES U))))
      (RETURN
       (COND ((EQUAL (LENGTH V) (LENGTH X)) RES)
             (T (NCONC RES (PNTH X (PLUS (LENGTH V) 1)))))))) 
(PUT 'LOCATE_N 'NUMBER-OF-ARGS 3) 
(PUT 'LOCATE_N 'DEFINED-ON-LINE '302) 
(PUT 'LOCATE_N 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'LOCATE_N 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LOCATE_N (X LST N)
    (PROG (N2 LST2 NTOT)
      (COND ((NULL LST) (RETURN NIL)))
      (SETQ LST2 LST)
      (SETQ NTOT 0)
     A
      (COND ((EQUAL N 0) (RETURN NTOT)))
      (SETQ N2 (LOCATE_MEMBER X LST2))
      (COND ((NULL N2) (RETURN NIL)))
      (SETQ LST2 (CDR (PNTH LST2 N2)))
      (SETQ NTOT (PLUS NTOT N2))
      (SETQ N (DIFFERENCE N 1))
      (GO A))) 
(PUT 'TERM2LISTPOWS 'NUMBER-OF-ARGS 1) 
(PUT 'TERM2LISTPOWS 'DEFINED-ON-LINE '320) 
(PUT 'TERM2LISTPOWS 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'TERM2LISTPOWS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TERM2LISTPOWS (U)
    (PROG ()
      (TRWRITE (LIST 'TERM2LISTPOWS "u= " U))
      (RETURN
       (COND ((NULL U) U) ((ATOM U) (LIST U))
             ((DOMAIN*P (CDR U)) (CONS (CAR U) (LIST (CDR U))))
             (T (CONS (CAR U) (TERM2LISTPOWS (CADR U)))))))) 
(PUT 'LISTPROD2TERM 'NUMBER-OF-ARGS 1) 
(PUT 'LISTPROD2TERM 'DEFINED-ON-LINE '332) 
(PUT 'LISTPROD2TERM 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'LISTPROD2TERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTPROD2TERM (U)
    (PROG (X RES)
      (COND ((NOT (LISTP U)) (REDERR "invalid argument to listprod2term")))
      (COND ((NULL U) (RETURN U)))
      (SETQ RES (CAR U))
      (SETQ RES
              (COND ((DOMAIN*P RES) (*D2Q RES))
                    ((SPP RES) (CONS (LIST (CONS RES 1)) 1))
                    ((STP RES) (CONS (LIST RES) 1)) ((SFP2 RES) (CONS RES 1))
                    (T RES)))
      (SETQ U (CDR U))
     A
      (COND ((NULL U) (RETURN RES)))
      (SETQ X (CAR U))
      (SETQ X
              (COND ((DOMAIN*P X) (*D2Q X))
                    ((SPP X) (CONS (LIST (CONS X 1)) 1))
                    ((STP X) (CONS (LIST X) 1)) ((SFP2 X) (CONS X 1)) (T X)))
      (SETQ U (CDR U))
      (SETQ RES (MULTSQ RES X))
      (GO A))) 
(PUT 'LOCATE_MEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'LOCATE_MEMBER 'DEFINED-ON-LINE '357) 
(PUT 'LOCATE_MEMBER 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'LOCATE_MEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LOCATE_MEMBER (U V)
    (COND ((NOT (MEMBER U V)) NIL) ((EQUAL U (CAR V)) 1)
          (T (PLUS 1 (LOCATE_MEMBER U (CDR V)))))) 
(GLOBAL '(DOMAINLIST*)) 
(PUT 'DOMAIN*P 'NUMBER-OF-ARGS 1) 
(PUT 'DOMAIN*P 'DEFINED-ON-LINE '363) 
(PUT 'DOMAIN*P 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'DOMAIN*P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DOMAIN*P (U)
    (OR (NULL U) (NUMBERP U) (AND (NOT (ATOM U)) (MEMQ (CAR U) DOMAINLIST*)))) 
(REMFLAG '(NONCOM) 'FLAGOP) 
(REMPROP 'NONCOM 'STAT) 
(DE NONCOMP2 (U)
    (COND ((ATOM U) (FLAGP U 'NONCOM)) (T (FLAGP (CAR U) 'NONCOM)))) 
(PUT 'NONCOMP2 'NUMBER-OF-ARGS 1) 
(PUT 'NONCOMP2 'DEFINED-ON-LINE '375) 
(PUT 'NONCOMP2 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NONCOMP2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'NONCOMP2 'INLINE
      '(LAMBDA (U)
         (COND ((ATOM U) (FLAGP U 'NONCOM)) (T (FLAGP (CAR U) 'NONCOM))))) 
(PUT 'NONCOM 'NUMBER-OF-ARGS 1) 
(PUT 'NONCOM 'DEFINED-ON-LINE '381) 
(PUT 'NONCOM 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NONCOM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NONCOM (U)
    (PROG (Y LISTE)
      (COND ((NOT (LISTP U)) (REDERR (LIST U "invalid argument to noncom"))))
      (PROG (X)
        (SETQ X U)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND
             ((NOT (IDP X)) (REDERR (LIST X "invalid argument to noncom"))))
            (NONCOM1 X)
            (SETQ LISTE (GET X 'NONCOMMUTES))
            (SETQ Y (DELETE X U))
            (PUT X 'NONCOMMUTES (DELETEMULT* (NCONC LISTE Y)))
            (COND
             ((EQUAL (GET X 'RTYPE) 'PHYSOP)
              (PROGN
               (NONCOM1 (ADJP X))
               (SETQ LISTE (GET (ADJP X) 'NONCOMMUTES))
               (SETQ Y
                       (DELETE (ADJP X)
                               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ J U)
                                 (COND ((NULL J) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (J) (ADJP J))
                                                   (CAR J))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ J (CDR J))
                                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS ((LAMBDA (J) (ADJP J)) (CAR J))
                                               NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))
               (PUT (ADJP X) 'NONCOMMUTES (DELETEMULT* (NCONC LISTE Y)))
               (NONCOM1 (INVP X))
               (SETQ LISTE (GET (INVP X) 'NONCOMMUTES))
               (SETQ Y
                       (DELETE (INVP X)
                               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                 (SETQ J U)
                                 (COND ((NULL J) (RETURN NIL)))
                                 (SETQ FORALL-RESULT
                                         (SETQ FORALL-ENDPTR
                                                 (CONS
                                                  ((LAMBDA (J) (INVP J))
                                                   (CAR J))
                                                  NIL)))
                                LOOPLABEL
                                 (SETQ J (CDR J))
                                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                                 (RPLACD FORALL-ENDPTR
                                         (CONS ((LAMBDA (J) (INVP J)) (CAR J))
                                               NIL))
                                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                 (GO LOOPLABEL))))
               (PUT (INVP X) 'NONCOMMUTES (DELETEMULT* (NCONC LISTE Y))))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN NIL))) 
(DEFLIST '((NONCOM RLIS)) 'STAT) 
(PUT 'NONCOMMUTING 'NUMBER-OF-ARGS 2) 
(PUT 'NONCOMMUTING 'DEFINED-ON-LINE '405) 
(PUT 'NONCOMMUTING 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NONCOMMUTING 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NONCOMMUTING (U V)
    (PROG (LIST RES)
      (SETQ U (COND ((ATOM U) U) (T (CAR U))))
      (SETQ V (COND ((ATOM V) V) (T (CAR V))))
      (COND
       ((NOT
         (AND (COND ((ATOM U) (FLAGP U 'NONCOM)) (T (FLAGP (CAR U) 'NONCOM)))
              (COND ((ATOM V) (FLAGP V 'NONCOM)) (T (FLAGP (CAR V) 'NONCOM)))))
        NIL)
       (T (PROGN (SETQ LIST (GET U 'NONCOMMUTES)) (SETQ RES (MEMBER V LIST)))))
      (RETURN RES))) 
(PUT 'NONCOMMUTINGTERM 'NUMBER-OF-ARGS 1) 
(PUT 'NONCOMMUTINGTERM 'DEFINED-ON-LINE '419) 
(PUT 'NONCOMMUTINGTERM 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NONCOMMUTINGTERM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NONCOMMUTINGTERM (U)
    (PROG (X Y)
      (COND ((OR (NULL U) (DOMAIN*P U) (SPP U)) (RETURN NIL)))
      (SETQ X (CAAR U))
      (SETQ U (CDR U))
     A
      (COND ((OR (NULL U) (DOMAIN*P U)) (RETURN NIL)))
      (SETQ Y (CAR U))
      (COND ((OR (NONCOMMUTINGF X (LIST Y)) (NONCOMMUTINGTERM Y)) (RETURN T)))
      (SETQ U (CDR U))
      (GO A))) 
(PUT 'NONCOMMUTINGF 'NUMBER-OF-ARGS 2) 
(PUT 'NONCOMMUTINGF 'DEFINED-ON-LINE '433) 
(PUT 'NONCOMMUTINGF 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NONCOMMUTINGF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NONCOMMUTINGF (X U)
    (COND ((DOMAIN*P U) NIL)
          (T
           (OR (NONCOMMUTING X (CAAAR U)) (NONCOMMUTINGF X (CDAR U))
               (NONCOMMUTINGF X (CDR U)))))) 
(PUT 'NONCOMMUTINGSP 'NUMBER-OF-ARGS 2) 
(PUT 'NONCOMMUTINGSP 'DEFINED-ON-LINE '440) 
(PUT 'NONCOMMUTINGSP 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NONCOMMUTINGSP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NONCOMMUTINGSP (U V)
    (COND ((OR (NULL U) (NULL V) (NUMBERP U) (NUMBERP V)) NIL)
          (T (NONCOMMUTING (CAR U) (CAR V))))) 
(PUT 'NONCOMMUTING_SPLIST 'NUMBER-OF-ARGS 2) 
(PUT 'NONCOMMUTING_SPLIST 'DEFINED-ON-LINE '445) 
(PUT 'NONCOMMUTING_SPLIST 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'NONCOMMUTING_SPLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NONCOMMUTING_SPLIST (U V)
    (COND ((OR (NULL V) (NULL U)) NIL)
          (T (OR (NONCOMMUTINGSP U (CAR V)) (NONCOMMUTING_SPLIST U (CDR V)))))) 
(PUT 'ORDP 'NUMBER-OF-ARGS 2) 
(PUT 'ORDP 'DEFINED-ON-LINE '458) 
(PUT 'ORDP 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'ORDP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORDP (U V)
    (COND ((NULL U) T) ((NULL V) NIL)
          ((VECTORP U) (COND ((VECTORP V) (ORDPV U V)) (T (ATOM V))))
          ((ATOM U)
           (COND
            ((ATOM V)
             (COND ((NUMBERP U) (COND ((NUMBERP V) (NOT (LESSP U V))) (T T)))
                   ((NUMBERP V) NIL) (T (ORDERP U V))))
            (T T)))
          ((ATOM V) NIL) ((EQUAL (CAR U) (CAR V)) (ORDPL (CDR U) (CDR V)))
          ((FLAGP (CAR U) 'NONCOM)
           (COND ((FLAGP (CAR V) 'NONCOM) (ORDP (CAR U) (CAR V))) (T T)))
          ((FLAGP (CAR V) 'NONCOM) NIL) (T (ORDP (CAR U) (CAR V))))) 
(PUT 'REORDOP 'NUMBER-OF-ARGS 2) 
(PUT 'REORDOP 'DEFINED-ON-LINE '481) 
(PUT 'REORDOP 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'REORDOP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REORDOP (U V)
    (COND ((NONCOMMUTING U V) T)
          ((AND (COND ((ATOM U) (FLAGP U 'NONCOM)) (T (FLAGP (CAR U) 'NONCOM)))
                (NOT
                 (COND ((ATOM V) (FLAGP V 'NONCOM))
                       (T (FLAGP (CAR V) 'NONCOM)))))
           NIL)
          ((AND (COND ((ATOM V) (FLAGP V 'NONCOM)) (T (FLAGP (CAR V) 'NONCOM)))
                (NOT
                 (COND ((ATOM U) (FLAGP U 'NONCOM))
                       (T (FLAGP (CAR U) 'NONCOM)))))
           T)
          (T (ORDOP U V)))) 
(PUT 'SUBS3F1 'NUMBER-OF-ARGS 3) 
(PUT 'SUBS3F1 'DEFINED-ON-LINE '498) 
(PUT 'SUBS3F1 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'SUBS3F1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBS3F1 (U L BOOL)
    (PROG (X Z)
      (SETQ Z (CONS NIL 1))
     A
      (COND ((NULL U) (RETURN Z))
            ((OR (ATOM U) (ATOM (CAR U))) (RETURN (ADDSQ Z (CONS U 1))))
            ((AND BOOL (OR (ATOM (CDAR U)) (ATOM (CAR (CDAR U))))) (GO C)))
      (SETQ X (COND (*MYMATCH (*SUBS3TNC (CAR U) L)) (T (SUBS3T (CAR U) L))))
      (COND ((OR (NOT BOOL) (NOT MCHFG*)) (GO B)))
      (SETQ MCHFG* NIL)
      (COND
       ((AND (EQUAL (CAR X) U) (EQUAL (CDR X) 1))
        (PROGN (SETQ X (CONS U 1)) (GO B)))
       ((NULL *RESUBS) (GO B)) ((OR *SUB2 POWLIS1*) (SETQ X (SUBS2Q X))))
      (SETQ X (SUBS3Q X))
     B
      (SETQ Z (ADDSQ Z X))
      (SETQ U (CDR U))
      (GO A)
     C
      (SETQ X (CONS (LIST (CAR U)) 1))
      (GO B))) 
(PUT '*SUBS3TNC 'NUMBER-OF-ARGS 2) 
(PUT '*SUBS3TNC 'DEFINED-ON-LINE '531) 
(PUT '*SUBS3TNC 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT '*SUBS3TNC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *SUBS3TNC (U V)
    (PROG (X Y RES MCHFG)
      (TRWRITE (LIST 'SUBS3TNC "before mchfg!*= " 'MCHFG*))
      (COND ((DOMAIN*P U) (RETURN (*D2Q U))))
      (COND ((KERNELP U) (RETURN (CONS (LIST (CONS (CONS U 1) 1)) 1))))
      (COND ((SPP U) (RETURN (CONS (LIST (CONS U 1)) 1))))
      (SETQ Y (ZERLEG U))
      (TRWRITE (LIST '*SUBS3TNC " y= " Y))
      (SETQ RES (CONS NIL 1))
     A
      (COND ((NULL Y) (PROGN (SETQ MCHFG* MCHFG) (RETURN RES))))
      (SETQ X (CAR Y))
      (SETQ Y (CDR Y))
      (SETQ RES (ADDSQ RES (SUBS3TNC X V)))
      (COND (MCHFG* (PROGN (SETQ MCHFG MCHFG*) (SETQ MCHFG* NIL))))
      (TRWRITE (LIST '*SUBS3TNC "res= " RES))
      (GO A))) 
(PUT 'ZERLEG 'NUMBER-OF-ARGS 1) 
(PUT 'ZERLEG 'DEFINED-ON-LINE '553) 
(PUT 'ZERLEG 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'ZERLEG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ZERLEG (U)
    (PROG (X RES)
      (COND ((NULL U) (RETURN U)))
      (COND ((DOMAIN*P U) (RETURN (LIST U))))
      (SETQ X (CAR U))
      (SETQ U (CDR U))
      (COND ((NULL U) (RETURN (LIST (LIST X)))))
      (COND ((DOMAIN*P U) (RETURN (LIST (LIST X U)))))
      (SETQ RES (ZERLEG (CAR U)))
      (SETQ RES
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J RES)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CONS X J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CONS X J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL (CDR U)) (RETURN RES))
            (T (RETURN (APPEND RES (ZERLEG (CONS X (CDR U))))))))) 
(PUT 'SUBS3TNC 'NUMBER-OF-ARGS 2) 
(PUT 'SUBS3TNC 'DEFINED-ON-LINE '570) 
(PUT 'SUBS3TNC 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'SUBS3TNC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBS3TNC (TERMLIST V)
    (PROG (TERMLIST2 TEMPL TEMP TEMPSP TEMPVAR FREETEMP RHS LHS BOOL BOOLP
           MATCHINGLIST X Y Z Z1 W W1 TERMLIST3 NA KA N K PREVTERML2 NABS)
      (SETQ MCHFG* NIL)
     A
      (COND ((NULL V) (RETURN (LISTPROD2TERM TERMLIST))))
      (SETQ TERMLIST2 (LISTOFVARNAMES TERMLIST))
      (SETQ TEMPL (CAR V))
      (SETQ V (CDR V))
      (SETQ RHS (NTH TEMPL 3))
      (SETQ BOOL (CDADR TEMPL))
      (SETQ BOOLP (CAADR TEMPL))
      (TRWRITE (LIST 'SUBS3TNC "bool= " BOOL " boolp= " BOOLP))
      (SETQ LHS (CAR TEMPL))
      (SETQ TEMP NIL)
      (SETQ FREETEMP NIL)
      (PROG (X)
        (SETQ X (REVERSE LHS))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND ((MEMQ (CAR X) FRLIS*) (SETQ FREETEMP (CONS X FREETEMP)))
                 (T (SETQ TEMP (CONS X TEMP)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ LHS NIL)
      (TRWRITE (LIST 'SUBS3TNC "temp= " TEMP "freetemp= " FREETEMP))
      (COND ((NULL TEMP) (GO B)))
      (SETQ BOOLP (COND ((EQUAL (LENGTH TEMP) 2) BOOLP) (T T)))
      (SETQ K 1)
      (SETQ NA 1)
      (SETQ NABS 0)
      (SETQ Z1 NIL)
      (SETQ MATCHINGLIST NIL)
     A1
      (COND ((GREATERP K (LENGTH TEMP)) (GO B)))
     AA
      (COND ((LESSP K NA) (GO A)))
      (SETQ TEMPSP (NTH TEMP K))
      (SETQ TEMPVAR (COND ((IDP (CAR TEMPSP)) (CAR TEMPSP)) (T (CAAR TEMPSP))))
     A2
      (SETQ N (LOCATE_MEMBER (CONS TEMPVAR 'FREE) TERMLIST2))
      (COND ((NUMBERP N) (GO AB)))
      (SETQ K (DIFFERENCE K 1))
      (SETQ Z1 NIL)
      (SETQ LHS (COND ((NULL LHS) LHS) (T (CDR LHS))))
      (SETQ TERMLIST2 PREVTERML2)
      (SETQ NABS (DIFFERENCE (LENGTH TERMLIST) (LENGTH TERMLIST2)))
      (GO AA)
     AB
      (SETQ TERMLIST2
              (NCONC
               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                 (SETQ K 1)
                 (COND ((MINUSP (DIFFERENCE (DIFFERENCE N 1) K)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (NTH TERMLIST2 K) NIL)))
                LOOPLABEL
                 (SETQ K (PLUS2 K 1))
                 (COND
                  ((MINUSP (DIFFERENCE (DIFFERENCE N 1) K))
                   (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (NTH TERMLIST2 K) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (CONS (CONS TEMPVAR 'USED) (PNTH TERMLIST2 (PLUS N 1)))))
      (SETQ X (NTH TERMLIST (PLUS N NABS)))
      (SETQ Z (MTCHP1* X TEMPSP BOOLP BOOL Z1))
      (COND ((NULL (CDR Z)) (GO A2)))
      (COND
       ((CAR Z)
        (PROGN
         (COND
          ((NOT (NONCOM2_SUBLIST (CAR Z) MATCHINGLIST))
           (SETQ MATCHINGLIST (NCONC MATCHINGLIST (CAR Z)))))
         (TRWRITE (LIST 'SUBS3TNC "matchinglist= " MATCHINGLIST))
         (PROG (Y)
           (SETQ Y (CAR Z))
          LAB
           (COND ((NULL Y) (RETURN NIL)))
           ((LAMBDA (Y)
              (PROGN
               (SETQ BOOL (SUBST (CDR Y) (CAR Y) BOOL))
               (SETQ TEMP (SUBST (CDR Y) (CAR Y) TEMP))))
            (CAR Y))
           (SETQ Y (CDR Y))
           (GO LAB))
         NIL)))
      (SETQ LHS (CONS X LHS))
      (TRWRITE (LIST 'SUBS3TNC "lhs= " LHS))
      (SETQ Z1 (CDR Z))
      (SETQ NA K)
      (SETQ K (PLUS K 1))
      (SETQ PREVTERML2 TERMLIST2)
      (SETQ TERMLIST2 (PNTH* TERMLIST2 (PLUS N 1)))
      (SETQ NABS (PLUS NABS N))
      (GO A1)
     B
      (COND
       ((NOT (NONCOM2_SUBLIST (CAR Z1) MATCHINGLIST))
        (SETQ MATCHINGLIST (NCONC MATCHINGLIST (CAR Z1)))))
      (COND
       ((EQUAL (LENGTH LHS) 2)
        (PROGN
         (SETQ X (CADR LHS))
         (SETQ Y (NTH TEMP 1))
         (COND
          ((NEQ (SETQ NA (CDR Y)) (SETQ KA (CDR X)))
           (PROGN
            (SETQ TERMLIST
                    (REPLSUBLIST
                     (LIST (CONS (CAR X) (DIFFERENCE KA NA)) (CONS (CAR X) NA))
                     (LIST (CONS (CAR X) KA)) TERMLIST))
            (SETQ W (LIST (CONS (CAR X) NA)))
            NIL))
          (T (SETQ W (LIST X))))
         (SETQ X (CAR LHS))
         (SETQ Y (NTH TEMP 2))
         (COND
          ((NEQ (SETQ NA (CDR Y)) (SETQ KA (CDR X)))
           (PROGN
            (SETQ TERMLIST
                    (REPLSUBLIST
                     (LIST (CONS (CAR X) NA) (CONS (CAR X) (DIFFERENCE KA NA)))
                     (LIST (CONS (CAR X) KA)) TERMLIST))
            (SETQ LHS (CONS (CONS (CAR X) NA) W))
            NIL))
          (T (SETQ LHS (CONS X W))))
         NIL)))
      (SETQ LHS (REVERSE LHS))
      (COND ((NULL (SETQ TERMLIST3 (NONCOM2_SUBLIST LHS TERMLIST))) (GO A)))
      (SETQ N (DIFFERENCE (LENGTH TERMLIST) (LENGTH TERMLIST3)))
      (SETQ TERMLIST
              (NCONC
               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                 (SETQ K 1)
                 (COND ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR (CONS (NTH TERMLIST K) NIL)))
                LOOPLABEL
                 (SETQ K (PLUS2 K 1))
                 (COND ((MINUSP (DIFFERENCE N K)) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR (CONS (NTH TERMLIST K) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               TERMLIST3))
      (SETQ NA (LENGTH FREETEMP))
      (COND ((EQUAL NA 0) (GO D)))
      (SETQ FREETEMP (REVERSE FREETEMP))
      (SETQ N
              (DIFFERENCE (LENGTH TERMLIST)
                          (LENGTH (MEMBER (CAR LHS) TERMLIST))))
      (COND ((LESSP N NA) (GO A)))
      (COND
       ((AND (EQUAL NA 1) (EQUAL (CDAR FREETEMP) 1))
        (PROGN
         (SETQ LHS TERMLIST)
         (SETQ MATCHINGLIST
                 (NCONC MATCHINGLIST
                        (LIST
                         (CONS (CAAR FREETEMP)
                               (*Q2A1
                                (LISTPROD2TERM
                                 (NCONC
                                  (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ K 1)
                                    (COND
                                     ((MINUSP (DIFFERENCE N K)) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS (NTH TERMLIST K)
                                                          NIL)))
                                   LOOPLABEL
                                    (SETQ K (PLUS2 K 1))
                                    (COND
                                     ((MINUSP (DIFFERENCE N K))
                                      (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS (NTH TERMLIST K) NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))
                                  (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ K (PLUS (LENGTH LHS) 1))
                                    (COND
                                     ((MINUSP
                                       (DIFFERENCE (LENGTH TERMLIST3) K))
                                      (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS (NTH TERMLIST3 K)
                                                          NIL)))
                                   LOOPLABEL
                                    (SETQ K (PLUS2 K 1))
                                    (COND
                                     ((MINUSP
                                       (DIFFERENCE (LENGTH TERMLIST3) K))
                                      (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS (NTH TERMLIST3 K) NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                                *NOSQ)))))
         NIL))
       (T
        (PROG (K)
          (SETQ K 1)
         LAB
          (COND ((MINUSP (DIFFERENCE NA K)) (RETURN NIL)))
          (PROGN
           (SETQ X (NTH TERMLIST (PLUS (DIFFERENCE N K) 1)))
           (SETQ Y (NTH FREETEMP K))
           (SETQ Z (MTCHP1 X Y BOOLP BOOL))
           (COND
            ((NOT (NONCOM2_SUBLIST (CAR Z) MATCHINGLIST))
             (SETQ MATCHINGLIST (NCONC MATCHINGLIST (CAR Z)))))
           (PROG (W)
             (SETQ W (CAR Z))
            LAB
             (COND ((NULL W) (RETURN NIL)))
             ((LAMBDA (W) (SETQ Y (SUBST (CDR W) (CAR W) Y))) (CAR W))
             (SETQ W (CDR W))
             (GO LAB))
           (SETQ LHS (CONS Y LHS))
           (COND
            ((NEQ (SETQ NA (CDR Y)) (SETQ KA (CDR X)))
             (PROGN
              (SETQ TERMLIST
                      (REPLSUBLIST
                       (LIST (CONS (CAR X) (DIFFERENCE KA NA))
                             (CONS (CAR X) NA))
                       (LIST (CONS (CAR X) KA)) TERMLIST))
              (SETQ N (PLUS N 1))
              NIL))))
          (SETQ K (PLUS2 K 1))
          (GO LAB))))
     D
      (TRWRITE (LIST 'SUBS3TNC "lhs= " LHS))
      (TRWRITE (LIST 'SUSB3TNC " termlist= " TERMLIST))
      (PROG (X)
        (SETQ X MATCHINGLIST)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ RHS (SUBST (CDR X) (CAR X) RHS))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ RHS (LIST (SIMP RHS)))
      (TRWRITE (LIST 'SUBS3TNC " rhs= " RHS))
      (SETQ TERMLIST (REPLSUBLIST RHS LHS TERMLIST))
      (TRWRITE (LIST 'SUBS3TNC "resulting termlist = " TERMLIST))
      (SETQ MCHFG* T)
      (RETURN (LISTPROD2TERM TERMLIST)))) 
(PUT 'MTCHP1* 'NUMBER-OF-ARGS 5) 
(PUT 'MTCHP1* 'DEFINED-ON-LINE '754) 
(PUT 'MTCHP1* 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'MTCHP1* 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MTCHP1* (U V BOOLP BOOL Z)
    (PROG (TEMP1 BOOL1 X Z1)
      (COND ((NULL Z) (RETURN (NCONC (LIST NIL) (MTCHP2 U V BOOLP BOOL)))))
     A
      (COND ((NULL Z) (RETURN (LIST NIL))))
      (SETQ X (CAR Z))
      (SETQ Z (CDR Z))
      (SETQ TEMP1 V)
      (SETQ BOOL1 BOOL)
      (PROG (W)
        (SETQ W X)
       LAB
        (COND ((NULL W) (RETURN NIL)))
        ((LAMBDA (W)
           (PROGN
            (SETQ TEMP1 (SUBST (CDR W) (CAR W) TEMP1))
            (SETQ BOOL1 (SUBST (CDR W) (CAR W) BOOL1))
            NIL))
         (CAR W))
        (SETQ W (CDR W))
        (GO LAB))
      (COND ((SETQ Z1 (MTCHP2 U TEMP1 BOOLP BOOL1)) (RETURN (CONS X Z1))))
      (GO A))) 
(PUT 'MTCHP2 'NUMBER-OF-ARGS 4) 
(PUT 'MTCHP2 'DEFINED-ON-LINE '778) 
(PUT 'MTCHP2 'DEFINED-IN-FILE 'HEPHYS/NONCOM2.RED) 
(PUT 'MTCHP2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MTCHP2 (U V BOOLP BOOL)
    (PROG (Z X RESLIST BOOL1 BOOL2)
      (SETQ Z (REVERSE (MTCHP1 U V BOOLP BOOL)))
      (COND ((EQUAL BOOL T) (RETURN Z)))
     A
      (COND ((NULL Z) (RETURN RESLIST)))
      (SETQ X (CAR Z))
      (SETQ Z (CDR Z))
      (SETQ BOOL1 BOOL)
      (PROG (W)
        (SETQ W X)
       LAB
        (COND ((NULL W) (RETURN NIL)))
        ((LAMBDA (W) (SETQ BOOL1 (SUBST (CDR W) (CAR W) BOOL1))) (CAR W))
        (SETQ W (CDR W))
        (GO LAB))
      (SETQ BOOL2 BOOL1)
      (PROG (W)
        (SETQ W FRLIS*)
       LAB
        (COND ((NULL W) (RETURN NIL)))
        ((LAMBDA (W) (SETQ BOOL2 (SUBST NIL W BOOL2))) (CAR W))
        (SETQ W (CDR W))
        (GO LAB))
      (TRWRITE (LIST 'MTCHP2 "bool1= " BOOL1 " bool2= " BOOL2))
      (COND ((AND (EQUAL BOOL2 BOOL1) (NULL (EVAL BOOL1))) (RETURN NIL))
            (T (SETQ RESLIST (CONS X RESLIST))))
      (GO A))) 
(ENDMODULE) 