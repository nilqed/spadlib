(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RANKSTAT)) 
(FLUID '(CURMODULE*)) 
(PUT 'TYPE_STAT 'NUMBER-OF-ARGS 0) 
(PUT 'TYPE_STAT 'DEFINED-ON-LINE '52) 
(PUT 'TYPE_STAT 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'TYPE_STAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE TYPE_STAT NIL
    ((LAMBDA (X) (PROGN (FLAG X 'TYPEID) (CONS 'TYPES X)))
     (REMCOMMA (XREAD NIL)))) 
(PUT 'TYPES 'STAT 'TYPE_STAT) 
(PUT 'SUBTYPE_RELS_STAT 'NUMBER-OF-ARGS 0) 
(PUT 'SUBTYPE_RELS_STAT 'DEFINED-ON-LINE '61) 
(PUT 'SUBTYPE_RELS_STAT 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'SUBTYPE_RELS_STAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE SUBTYPE_RELS_STAT NIL (CONS 'SUBTYPERELS (REMCOMMA (XREAD 'LAMBDA)))) 
(PUT 'SUBTYPES 'STAT 'SUBTYPE_RELS_STAT) 
(PUT 'N_FORMSUBTYPERELS 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMSUBTYPERELS 'DEFINED-ON-LINE '67) 
(PUT 'N_FORMSUBTYPERELS 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'N_FORMSUBTYPERELS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMSUBTYPERELS (U VARS)
    (LIST 'NOVAL
          (CONS 'PROGN
                (ACONC
                 (PROG (J FORALL-RESULT FORALL-ENDPTR)
                   (SETQ J (CDR U))
                  STARTOVER
                   (COND ((NULL J) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (J) (N_FORMSUBTYPERELS1 J VARS)) (CAR J)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ J (CDR J))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL J) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (J) (N_FORMSUBTYPERELS1 J VARS)) (CAR J)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ J (CDR J))
                   (GO LOOPLABEL))
                 ''(NOVAL NIL))))) 
(PUT 'N_FORMSUBTYPERELS1 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMSUBTYPERELS1 'DEFINED-ON-LINE '72) 
(PUT 'N_FORMSUBTYPERELS1 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'N_FORMSUBTYPERELS1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMSUBTYPERELS1 (U VARS)
    (COND ((NULL (EQCAR U 'LESSP)) NIL)
          (T
           (APPEND
            (N_FORMSUBTYPERELS2
             (COND ((ATOM (CADDR U)) (LIST (CADDR U))) (T (CADDR U))) (CADR U)
             VARS)
            (N_FORMSUBTYPERELS1 (CADR U) VARS))))) 
(PUT 'N_FORMSUBTYPERELS2 'NUMBER-OF-ARGS 3) 
(PUT 'N_FORMSUBTYPERELS2 'DEFINED-ON-LINE '78) 
(PUT 'N_FORMSUBTYPERELS2 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'N_FORMSUBTYPERELS2 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE N_FORMSUBTYPERELS2 (U V VARS)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
     STARTOVER
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (J)
                 (PROG (X)
                   (SETQ X
                           (COND ((ATOM V) (LIST V))
                                 ((EQ (CAR V) 'LESSP)
                                  (COND ((ATOM (CADDR V)) (LIST (CADDR V)))
                                        (T (FLAT_TYPEL (CADDR V)))))
                                 (T (FLAT_TYPEL V))))
                   (RETURN
                    (CONS
                     (LIST 'PUT (MKQUOTE J) ''TYPETREE
                           (LIST 'UNION (LIST 'GET (MKQUOTE J) ''TYPETREE)
                                 (MKQUOTE X)))
                     (PROG (K FORALL-RESULT FORALL-ENDPTR)
                       (SETQ K X)
                       (COND ((NULL K) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (K)
                                           (LIST 'PUT (MKQUOTE K) ''UPTREE
                                                 (LIST 'UNION
                                                       (LIST 'GET (MKQUOTE K)
                                                             ''UPTREE)
                                                       (MKQUOTE (LIST J)))))
                                         (CAR K))
                                        NIL)))
                      LOOPLABEL
                       (SETQ K (CDR K))
                       (COND ((NULL K) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (K)
                                   (LIST 'PUT (MKQUOTE K) ''UPTREE
                                         (LIST 'UNION
                                               (LIST 'GET (MKQUOTE K) ''UPTREE)
                                               (MKQUOTE (LIST J)))))
                                 (CAR K))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ J (CDR J))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (J)
                 (PROG (X)
                   (SETQ X
                           (COND ((ATOM V) (LIST V))
                                 ((EQ (CAR V) 'LESSP)
                                  (COND ((ATOM (CADDR V)) (LIST (CADDR V)))
                                        (T (FLAT_TYPEL (CADDR V)))))
                                 (T (FLAT_TYPEL V))))
                   (RETURN
                    (CONS
                     (LIST 'PUT (MKQUOTE J) ''TYPETREE
                           (LIST 'UNION (LIST 'GET (MKQUOTE J) ''TYPETREE)
                                 (MKQUOTE X)))
                     (PROG (K FORALL-RESULT FORALL-ENDPTR)
                       (SETQ K X)
                       (COND ((NULL K) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (K)
                                           (LIST 'PUT (MKQUOTE K) ''UPTREE
                                                 (LIST 'UNION
                                                       (LIST 'GET (MKQUOTE K)
                                                             ''UPTREE)
                                                       (MKQUOTE (LIST J)))))
                                         (CAR K))
                                        NIL)))
                      LOOPLABEL
                       (SETQ K (CDR K))
                       (COND ((NULL K) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (K)
                                   (LIST 'PUT (MKQUOTE K) ''UPTREE
                                         (LIST 'UNION
                                               (LIST 'GET (MKQUOTE K) ''UPTREE)
                                               (MKQUOTE (LIST J)))))
                                 (CAR K))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))))))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ J (CDR J))
      (GO LOOPLABEL))) 
(PUT 'FLAT_TYPEL 'NUMBER-OF-ARGS 1) 
(PUT 'FLAT_TYPEL 'DEFINED-ON-LINE '95) 
(PUT 'FLAT_TYPEL 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'FLAT_TYPEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FLAT_TYPEL (U)
    (CONS (CAR U)
          (COND ((NULL (ATOM (CADR U))) (FLAT_TYPEL (CADR U))) (T (CDR U))))) 
(PUT 'SUBTYPERELS 'N_FORMFN 'N_FORMSUBTYPERELS) 
(FLAG '(SUBTYPERELS) 'ALWAYS_NFORM) 
(NEWTOK '((- >) MAPPED_TO)) 
(PUT 'RANKS_STAT 'NUMBER-OF-ARGS 0) 
(PUT 'RANKS_STAT 'DEFINED-ON-LINE '104) 
(PUT 'RANKS_STAT 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'RANKS_STAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE RANKS_STAT NIL
    (PROG (PROPS OPER ARITY COARITY RANKS1)
     LOOP
      (SETQ OPER (XREAD 'FOR))
      (COND ((ATOM OPER) (SETQ OPER (LIST '*NULLARY* OPER))))
      (FLAG '(MAPPED_TO) 'DELIM)
      (SETQ ARITY (XREAD NIL))
      (COND ((ATOM ARITY) (SETQ ARITY (LIST ARITY)))
            (T (SETQ ARITY (CDR ARITY))))
      (REMFLAG '(MAPPED_TO) 'DELIM)
      (SETQ COARITY (XREAD 'GROUP))
      (COND ((ATOM COARITY) (SETQ PROPS NIL))
            ((EQ (CAR COARITY) 'WHEN)
             (PROGN
              (SETQ PROPS (CONS 'WHEN (CDDR COARITY)))
              (SETQ COARITY (CADR COARITY))))
            ((EQ (CADR COARITY) 'SYMMETRIC)
             (PROGN (SETQ COARITY (CAR COARITY)) (SETQ PROPS 'SYMMETRIC)))
            (T
             (PROGN
              (PRIN2 "unimplemented property: ")
              (PRIN2T (CADR COARITY))
              (SETQ COARITY (CAR COARITY)))))
      (SETQ RANKS1 (ACONC RANKS1 (LIST OPER ARITY COARITY PROPS)))
      (COND ((EQ CURSYM* '*COMMA*) (GO LOOP)))
      (RETURN (CONS 'RANKS RANKS1)))) 
(PUT 'RANKS 'STAT 'RANKS_STAT) 
(PUT 'N_FORMRANKS 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMRANKS 'DEFINED-ON-LINE '133) 
(PUT 'N_FORMRANKS 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'N_FORMRANKS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMRANKS (U VARS)
    (PROG (R E_PTR RNK ARITY COARITY PROPS OP OPVARS Z N)
      (SETQ R (LIST 'NOVAL (SETQ E_PTR (LIST 'PROGN))))
     A
      (SETQ U (CDR U))
      (COND ((NULL U) (PROGN (RPLACD E_PTR (LIST ''(NOVAL NIL))) (RETURN R))))
      (SETQ RNK (CAR U))
      (SETQ OP (CAAR RNK))
      (SETQ OPVARS (CDAR RNK))
      (SETQ ARITY (CADR RNK))
      (SETQ COARITY (CADDR RNK))
      (SETQ RNK (CDDDR RNK))
      (SETQ PROPS (CAR RNK))
      (COND ((CDR RNK) (SETQ RNK (CADR RNK))) (T (SETQ RNK NIL)))
      (COND
       ((EQ OP '*NULLARY*)
        (PROGN
         (FLAG ARITY 'DEFINING)
         (COND
          ((EQCAR PROPS 'WHEN)
           (SETQ PROPS
                   (LIST
                    (FORM1 (CADR PROPS) (APPEND (PAIR OPVARS ARITY) VARS)
                           'SYMBOLIC))))
          (T (REDERR "unconditional constraint not supported")))
         (REMFLAG ARITY 'DEFINING)
         (SETQ PROPS (LIST '(X1) (SUBLA (PAIR OPVARS '((VALUE X1))) PROPS)))
         (SETQ Z (MK_NULLARYFNS ARITY COARITY PROPS))
         (SETQ E_PTR (CDR (RPLACD E_PTR (LIST (CAR Z)))))
         (SETQ E_PTR (CDR (RPLACD E_PTR (CDR Z))))
         (GO A))))
      (COND
       ((EQ PROPS 'SYMMETRIC)
        (PROGN (PUT OP 'SYMMETRICFN ARITY) (SETQ PROPS NIL))))
      (COND
       ((EQCAR PROPS 'WHEN)
        (SETQ PROPS
                (LIST
                 (LIST 'VALUE
                       (VALUE
                        (N_FORM1 (CADR PROPS)
                         (APPEND (PAIR OPVARS ARITY) VARS)))))))
       (T (SETQ PROPS (LIST T))))
      (SETQ N 0)
      (SETQ Z
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J OPVARS)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (CONS J
                                          (INTERN
                                           (COMPRESS
                                            (APPEND (EXPLODE 'X)
                                                    (EXPLODE
                                                     (SETQ N (PLUS N 1))))))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (CONS J
                                  (INTERN
                                   (COMPRESS
                                    (APPEND (EXPLODE 'X)
                                            (EXPLODE (SETQ N (PLUS N 1))))))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ PROPS (SUBLA Z (LIST OPVARS PROPS)))
      (PROG (RANKFN)
        (SETQ RANKFN (MK_RANKFNS OP COARITY ARITY PROPS RNK))
       LAB
        (COND ((NULL RANKFN) (RETURN NIL)))
        ((LAMBDA (RANKFN) (SETQ E_PTR (CDR (RPLACD E_PTR (LIST RANKFN)))))
         (CAR RANKFN))
        (SETQ RANKFN (CDR RANKFN))
        (GO LAB))
      (GO A))) 
(PUT 'RANKS 'N_FORMFN 'N_FORMRANKS) 
(FLAG '(RANKS) 'ALWAYS_NFORM) 
(PUT 'MK_RANKFNS 'NUMBER-OF-ARGS 5) 
(PUT 'MK_RANKFNS 'DEFINED-ON-LINE '183) 
(PUT 'MK_RANKFNS 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'MK_RANKFNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_RANKFNS (OP COARITY ARITY PROPS ALTOP)
    (PROG (X DISAMBOP DISAMBOP2 RANKFNS N)
      (SETQ N 0)
      (SETQ N 0)
      (SETQ DISAMBOP
              (MKRANKEDNAME OP ARITY
               (COND ((EQ (CAADR PROPS) T) NIL) (T COARITY))))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J ARITY)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (INTERN
                                     (COMPRESS
                                      (APPEND (EXPLODE J)
                                              (EXPLODE (SETQ N (PLUS N 1)))))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (INTERN
                             (COMPRESS
                              (APPEND (EXPLODE J)
                                      (EXPLODE (SETQ N (PLUS N 1)))))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RANKFNS
              (CONS
               (LIST 'DE DISAMBOP X
                     (LIST 'MKOBJECT
                           (COND
                            (ALTOP
                             (CONS ALTOP
                                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ J X)
                                     (COND ((NULL J) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (J)
                                                         (LIST 'VALUE J))
                                                       (CAR J))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ J (CDR J))
                                     (COND ((NULL J) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (J) (LIST 'VALUE J))
                                               (CAR J))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL))))
                            ((EQ OP 'SETQ)
                             (CONS 'SET
                                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ J X)
                                     (COND ((NULL J) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (J)
                                                         (LIST 'VALUE J))
                                                       (CAR J))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ J (CDR J))
                                     (COND ((NULL J) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (J) (LIST 'VALUE J))
                                               (CAR J))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL))))
                            (OP
                             (CONS OP
                                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ J X)
                                     (COND ((NULL J) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             (SETQ FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (J)
                                                         (LIST 'VALUE J))
                                                       (CAR J))
                                                      NIL)))
                                    LOOPLABEL
                                     (SETQ J (CDR J))
                                     (COND ((NULL J) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             (CONS
                                              ((LAMBDA (J) (LIST 'VALUE J))
                                               (CAR J))
                                              NIL))
                                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                     (GO LOOPLABEL))))
                            (T (CONS 'VALUE X)))
                           (MKQUOTE COARITY)))
               RANKFNS))
      (SETQ RANKFNS
              (CONS
               (LIST 'ADDRANK0 (MKQUOTE OP) (MKQUOTE ARITY)
                     (MKQUOTE
                      (LIST (CAR PROPS)
                            (APPEND (CADR PROPS)
                                    (LIST
                                     (MKQUOTE (LIST DISAMBOP COARITY)))))))
               RANKFNS))
      (COND ((NULL (SYMMETRICP (CONS OP ARITY))) (RETURN RANKFNS)))
      (COND
       ((NEQ (LENGTH ARITY) 2)
        (REDERR "only binary symmetric functions are supported")))
      (COND
       ((AND (EQ (CAR ARITY) (CADR ARITY)) (EQ (CAADR PROPS) T))
        (RETURN RANKFNS)))
      (SETQ DISAMBOP2
              (MKRANKEDNAME OP (REVERSE ARITY)
               (COND
                ((NULL (EQ (CAADR PROPS) T))
                 (COND
                  ((EQ (CAR ARITY) (CADR ARITY))
                   (INTERN (COMPRESS (APPEND (EXPLODE '!) (EXPLODE COARITY)))))
                  (T COARITY)))
                (T NIL))))
      (SETQ RANKFNS
              (CONS (LIST 'DE DISAMBOP2 (REVERSE X) (CONS DISAMBOP X))
                    RANKFNS))
      (SETQ RANKFNS
              (CONS
               (LIST 'ADDRANK0 (MKQUOTE OP) (MKQUOTE (REVERSE ARITY))
                     (MKQUOTE
                      (LIST (CAR PROPS)
                            (APPEND
                             (COND ((EQ (CAR ARITY) (CADR ARITY)) (CADR PROPS))
                                   (T
                                    (SUBLA
                                     (PAIR (CAR PROPS) (REVERSE (CAR PROPS)))
                                     (CADR PROPS))))
                             (LIST (MKQUOTE (LIST DISAMBOP2 COARITY)))))))
               RANKFNS))
      (RETURN RANKFNS))) 
(PUT 'SYMMETRICP 'NUMBER-OF-ARGS 1) 
(PUT 'SYMMETRICP 'DEFINED-ON-LINE '230) 
(PUT 'SYMMETRICP 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'SYMMETRICP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYMMETRICP (U)
    ((LAMBDA (X)
       (AND X
            (OR (AND (XTYPE1 (CADR U) (CAR X)) (XTYPE1 (CADDR U) (CADR X)))
                (AND (XTYPE1 (CADDR U) (CAR X)) (XTYPE1 (CADR U) (CADR X))))))
     (GET (CAR U) 'SYMMETRICFN))) 
(PUT 'MK_NULLARYFNS 'NUMBER-OF-ARGS 3) 
(PUT 'MK_NULLARYFNS 'DEFINED-ON-LINE '235) 
(PUT 'MK_NULLARYFNS 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'MK_NULLARYFNS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MK_NULLARYFNS (ARITY COARITY PROPS)
    (PROG (X DISAMBOP N)
      (SETQ N 0)
      (SETQ DISAMBOP (MKRANKEDNAME NIL ARITY COARITY))
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J ARITY)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (INTERN
                                     (COMPRESS
                                      (APPEND (EXPLODE J)
                                              (EXPLODE (SETQ N (PLUS N 1)))))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (INTERN
                             (COMPRESS
                              (APPEND (EXPLODE J)
                                      (EXPLODE (SETQ N (PLUS N 1)))))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN
       (LIST
        (LIST 'ADDNULLARY (MKQUOTE (CAR ARITY))
              (MKQUOTE
               (LIST (CAR PROPS)
                     (APPEND (CADR PROPS)
                             (LIST (MKQUOTE (LIST DISAMBOP COARITY)))))))
        (LIST 'DE DISAMBOP X
              (CONS 'LIST (CONS (MKQUOTE COARITY) (LIST (CONS 'VALUE X))))))))) 
(PUT 'INSTALL 'STAT 'INSTALLSTAT) 
(PUT 'INSTALLSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'INSTALLSTAT 'DEFINED-ON-LINE '253) 
(PUT 'INSTALLSTAT 'DEFINED-IN-FILE 'REDUCE4/RANKSTAT.RED) 
(PUT 'INSTALLSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE INSTALLSTAT NIL
    (PROG (MODE OPRNAME X Y)
      (SETQ MODE 'GENERIC)
      (SETQ OPRNAME (SCAN))
      (COND
       ((NULL (IDP OPRNAME)) (PROGN (TYPERR OPRNAME "install name") (GO C))))
      (SCAN)
      (SETQ X (ERRORSET* (LIST 'READ_PARAM_LIST NIL) NIL))
      (COND ((ERRORP X) (GO C)))
      (SETQ X (CAR X))
      (COND ((EQ CURSYM* '*COLON*) (SETQ MODE (READ_TYPE4))))
      (COND ((NULL (EQ CURSYM* 'MAPPED_TO)) (GO C)))
      (SETQ Y (SCAN))
      (COND ((NOT (EQ (SCAN) '*SEMICOL*)) (GO C)))
      (RETURN
       (LIST 'RANKS
             (LIST
              (CONS OPRNAME
                    (PROG (J FORALL-RESULT FORALL-ENDPTR)
                      (SETQ J X)
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
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))
              MODE NIL Y)))
     C
      (ERRORSET* '(SYMERR 'INSTALL T) NIL))) 
(ENDMODULE) 