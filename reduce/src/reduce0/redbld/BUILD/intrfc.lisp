(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INTRFC)) 
(FLUID '(*GETDECS)) 
(PUT 'GENTRAN 'STAT 'GENSTAT) 
(PUT 'GENTRANIN 'STAT 'GENINSTAT) 
(PUT 'GENTRANOUT 'STAT 'GENOUTSTAT) 
(PUT 'GENTRANSHUT 'STAT 'GENSHUTSTAT) 
(PUT 'GENTRANPUSH 'STAT 'GENPUSHSTAT) 
(PUT 'GENTRANPOP 'STAT 'GENPOPSTAT) 
(PUT 'GENTRAN 'FORMFN 'FORMGENTRAN) 
(PUT 'GENTRANIN 'FORMFN 'FORMGENTRAN) 
(PUT 'GENTRANOUTPUSH 'FORMFN 'FORMGENTRAN) 
(PUT 'GENTRANSHUT 'FORMFN 'FORMGENTRAN) 
(PUT 'GENTRANPOP 'FORMFN 'FORMGENTRAN) 
(PUT 'DECLARE 'STAT 'DECLARESTAT) 
(PUT 'LITERAL 'STAT 'LITERALSTAT) 
(NEWTOK '((|:| |:| =) LSETQ)) 
(INFIX (LIST 'LSETQ)) 
(NEWTOK '((|:| = |:|) RSETQ)) 
(INFIX (LIST 'RSETQ)) 
(NEWTOK '((|:| |:| = |:|) LRSETQ)) 
(INFIX (LIST 'LRSETQ)) 
(FLAG '(GENDECS) 'OPFN) 
(FLUID '(*GENDECS)) 
(SETQ *GENDECS T) 
(PUT 'GENDECS 'SIMPFG '((NIL) (T (GENDECS NIL)))) 
(SWITCH (LIST 'GENDECS)) 
(FLUID '(*KEEPDECS)) 
(SETQ *KEEPDECS NIL) 
(SWITCH (LIST 'KEEPDECS)) 
(FLUID '(*GENTRANOPT *GENTRANSEG *PERIOD)) 
(SETQ *GENTRANSEG T) 
(SWITCH (LIST 'GENTRANSEG)) 
(GLOBAL '(GENTRANLANG*)) 
(SHARE (LIST 'GENTRANLANG*)) 
(SETQ GENTRANLANG* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 'FORTRAN)) 
(GLOBAL
 '(*TERM* *STDIN* *STDOUT* *INSTK* *CURRIN* *OUTSTK* *CURROUT* *OUTCHANL*)) 
(SETQ *TERM* (CONS T NIL)) 
(SETQ *STDIN* *TERM*) 
(SETQ *STDOUT* *TERM*) 
(SETQ *INSTK* (LIST *STDIN*)) 
(SETQ *CURRIN* (CAR *INSTK*)) 
(SETQ *OUTSTK* (LIST *STDOUT*)) 
(SETQ *CURROUT* (CAR *OUTSTK*)) 
(SETQ *OUTCHANL* (LIST (CDR *CURROUT*))) 
(GLOBAL '(*DO* *FOR*)) 
(SETQ *DO* 'DO) 
(SETQ *FOR* 'FOR) 
(GLOBAL '(*LISPSTMTOPS*)) 
(SETQ *LISPSTMTOPS* (CONS *FOR* *LISPSTMTOPS*)) 
(GLOBAL '(CURSYM* *VARS*)) 
(FLUID '(*MODE)) 
(PUT 'GENSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'GENSTAT 'DEFINED-ON-LINE '128) 
(PUT 'GENSTAT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GENSTAT NIL
    (PROG (STMT)
      (FLAG '(OUT) 'DELIM)
      (SETQ STMT (XREAD T))
      (REMFLAG '(OUT) 'DELIM)
      (COND ((EQ CURSYM* 'OUT) (RETURN (LIST 'GENTRAN STMT (READFARGS))))
            ((ENDOFSTMTP) (RETURN (LIST 'GENTRAN STMT NIL)))
            (T (GENTRANERR 'E NIL "INVALID SYNTAX" NIL))))) 
(PUT 'GENINSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'GENINSTAT 'DEFINED-ON-LINE '148) 
(PUT 'GENINSTAT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENINSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GENINSTAT NIL
    (PROG (F1 F2)
      (FLAG '(OUT) 'DELIM)
      (SETQ F1 (XREAD NIL))
      (COND ((ATOM F1) (SETQ F1 (LIST F1))) (T (SETQ F1 (CDR F1))))
      (REMFLAG '(OUT) 'DELIM)
      (COND ((EQ CURSYM* 'OUT) (SETQ F2 (READFARGS))))
      (RETURN (LIST 'GENTRANIN F1 F2)))) 
(PUT 'GENOUTSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'GENOUTSTAT 'DEFINED-ON-LINE '166) 
(PUT 'GENOUTSTAT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENOUTSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GENOUTSTAT NIL (LIST 'GENTRANOUTPUSH (READFARGS))) 
(PUT 'GENSHUTSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'GENSHUTSTAT 'DEFINED-ON-LINE '173) 
(PUT 'GENSHUTSTAT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENSHUTSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GENSHUTSTAT NIL (LIST 'GENTRANSHUT (READFARGS))) 
(PUT 'GENPUSHSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'GENPUSHSTAT 'DEFINED-ON-LINE '180) 
(PUT 'GENPUSHSTAT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENPUSHSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GENPUSHSTAT NIL (LIST 'GENTRANOUTPUSH (READFARGS))) 
(PUT 'GENPOPSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'GENPOPSTAT 'DEFINED-ON-LINE '187) 
(PUT 'GENPOPSTAT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENPOPSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE GENPOPSTAT NIL (LIST 'GENTRANPOP (READFARGS))) 
(NEWTOK '((|:| |:|) RANGE)) 
(PUT 'DECLARESTAT 'NUMBER-OF-ARGS 0) 
(PUT 'DECLARESTAT 'DEFINED-ON-LINE '199) 
(PUT 'DECLARESTAT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'DECLARESTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DECLARESTAT NIL
    (PROG (RES VARLST TYPE)
      (SCAN)
      (PUT 'RANGE 'INFIX 4)
      (PUT 'RANGE 'OP '((4 4)))
      (COND
       ((EQ CURSYM* '*STARTGROUP*)
        (PROGN
         (SCAN)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NEQ CURSYM* '*ENDGROUP*)) (RETURN NIL)))
           (PROGN
            (SETQ VARLST (LIST (XREAD1 'FOR)))
            (PROG ()
             WHILELABEL
              (COND ((NOT (NEQ CURSYM* '*COLON*)) (RETURN NIL)))
              (SETQ VARLST (APPEND VARLST (LIST (XREAD 'FOR))))
              (GO WHILELABEL))
            (SETQ TYPE (DECLARESTAT1))
            (SETQ RES (APPEND RES (LIST (CONS TYPE VARLST))))
            (COND ((EQ CURSYM* '*SEMICOL*) (SCAN))))
           (GO WHILELABEL))
         (SCAN)))
       (T
        (PROGN
         (SETQ VARLST (LIST (XREAD1 'FOR)))
         (PROG ()
          WHILELABEL
           (COND ((NOT (NEQ CURSYM* '*COLON*)) (RETURN NIL)))
           (SETQ VARLST (APPEND VARLST (LIST (XREAD 'FOR))))
           (GO WHILELABEL))
         (SETQ TYPE (DECLARESTAT1))
         (SETQ RES (LIST (CONS TYPE VARLST)))
         NIL)))
      (COND ((NOT (ENDOFSTMTP)) (GENTRANERR 'E NIL "INVALID SYNTAX" NIL)))
      (REMPROP 'RANGE 'INFIX)
      (REMPROP 'RANGE 'OP)
      (RETURN (CONS 'DECLARE RES)))) 
(PUT 'DECLARESTAT1 'NUMBER-OF-ARGS 0) 
(PUT 'DECLARESTAT1 'DEFINED-ON-LINE '246) 
(PUT 'DECLARESTAT1 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'DECLARESTAT1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE DECLARESTAT1 NIL
    (PROG (RES)
      (SCAN)
      (COND ((ENDOFSTMTP) (RETURN NIL)))
      (COND
       ((EQ CURSYM* 'IMPLICIT)
        (PROGN
         (SCAN)
         (SETQ RES
                 (INTERN
                  (COMPRESS
                   (APPEND (EXPLODE '|IMPLICIT |) (EXPLODE CURSYM*)))))))
       (T (SETQ RES CURSYM*)))
      (SCAN)
      (COND
       ((EQ CURSYM* 'TIMES)
        (PROGN
         (SCAN)
         (COND
          ((NUMBERP CURSYM*)
           (PROGN
            (SETQ RES
                    (INTERN
                     (COMPRESS
                      (APPEND (APPEND (EXPLODE RES) (EXPLODE '*))
                              (EXPLODE CURSYM*)))))
            (SCAN)))
          (T (GENTRANERR 'E NIL "INVALID SYNTAX" NIL))))))
      (RETURN RES))) 
(PUT 'LITERALSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'LITERALSTAT 'DEFINED-ON-LINE '276) 
(PUT 'LITERALSTAT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'LITERALSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LITERALSTAT NIL
    (PROG (RES)
      (PROG ()
       REPEATLABEL
        (SETQ RES (APPEND RES (LIST (XREAD T))))
        (COND ((NOT (ENDOFSTMTP)) (GO REPEATLABEL))))
      (COND ((ATOM RES) (RETURN (LIST 'LITERAL RES)))
            ((EQ (CAR RES) '*COMMA*) (RETURN (RPLACA RES 'LITERAL)))
            (T (RETURN (CONS 'LITERAL RES)))))) 
(PUT 'SYM-GENTRAN 'NUMBER-OF-ARGS 1) 
(PUT 'SYM-GENTRAN 'DEFINED-ON-LINE '299) 
(PUT 'SYM-GENTRAN 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'SYM-GENTRAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYM-GENTRAN (FORM)
    (LISPEVAL (FORMGENTRAN (LIST 'GENTRAN FORM NIL) *VARS* *MODE))) 
(PUT 'SYM-GENTRANIN 'NUMBER-OF-ARGS 1) 
(PUT 'SYM-GENTRANIN 'DEFINED-ON-LINE '302) 
(PUT 'SYM-GENTRANIN 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'SYM-GENTRANIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYM-GENTRANIN (FLIST)
    (COND
     (FLIST
      (LISPEVAL
       (FORMGENTRAN
        (LIST 'GENTRANIN (COND ((ATOM FLIST) (LIST FLIST)) (T FLIST)) NIL)
        *VARS* *MODE))))) 
(PUT 'SYM-GENTRANOUT 'NUMBER-OF-ARGS 1) 
(PUT 'SYM-GENTRANOUT 'DEFINED-ON-LINE '309) 
(PUT 'SYM-GENTRANOUT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'SYM-GENTRANOUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYM-GENTRANOUT (FLIST)
    (LISPEVAL
     (FORMGENTRAN
      (LIST 'GENTRANOUTPUSH (COND ((ATOM FLIST) (LIST FLIST)) (T FLIST)))
      *VARS* *MODE))) 
(PUT 'SYM-GENTRANSHUT 'NUMBER-OF-ARGS 1) 
(PUT 'SYM-GENTRANSHUT 'DEFINED-ON-LINE '314) 
(PUT 'SYM-GENTRANSHUT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'SYM-GENTRANSHUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYM-GENTRANSHUT (FLIST)
    (LISPEVAL
     (FORMGENTRAN
      (LIST 'GENTRANSHUT (COND ((ATOM FLIST) (LIST FLIST)) (T FLIST))) *VARS*
      *MODE))) 
(PUT 'SYM-GENTRANPUSH 'NUMBER-OF-ARGS 1) 
(PUT 'SYM-GENTRANPUSH 'DEFINED-ON-LINE '319) 
(PUT 'SYM-GENTRANPUSH 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'SYM-GENTRANPUSH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYM-GENTRANPUSH (FLIST)
    (LISPEVAL
     (FORMGENTRAN
      (LIST 'GENTRANOUTPUSH (COND ((ATOM FLIST) (LIST FLIST)) (T FLIST)))
      *VARS* *MODE))) 
(PUT 'SYM-GENTRANPOP 'NUMBER-OF-ARGS 1) 
(PUT 'SYM-GENTRANPOP 'DEFINED-ON-LINE '324) 
(PUT 'SYM-GENTRANPOP 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'SYM-GENTRANPOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYM-GENTRANPOP (FLIST)
    (LISPEVAL
     (FORMGENTRAN
      (LIST 'GENTRANPOP (COND ((ATOM FLIST) (LIST FLIST)) (T FLIST))) *VARS*
      *MODE))) 
(PUT 'FORMGENTRAN 'NUMBER-OF-ARGS 3) 
(PUT 'FORMGENTRAN 'DEFINED-ON-LINE '335) 
(PUT 'FORMGENTRAN 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'FORMGENTRAN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMGENTRAN (U VARS MODE)
    (CONS (CAR U)
          (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
            (SETQ ARG (CDR U))
            (COND ((NULL ARG) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ARG) (FORMGENTRAN1 ARG VARS MODE))
                              (CAR ARG))
                             NIL)))
           LOOPLABEL
            (SETQ ARG (CDR ARG))
            (COND ((NULL ARG) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (ARG) (FORMGENTRAN1 ARG VARS MODE)) (CAR ARG))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'FORMGENTRAN1 'NUMBER-OF-ARGS 3) 
(PUT 'FORMGENTRAN1 'DEFINED-ON-LINE '338) 
(PUT 'FORMGENTRAN1 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'FORMGENTRAN1 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMGENTRAN1 (U VARS MODE)
    (PROGN
     (COND ((EQCAR U 'PROCEDURE) (SETQ U (STRIP_PROCEDURE_INFO U))))
     (COND
      ((AND (PAIRP U) (EQ (CAR U) '|:DN:|))
       (MKQUOTE
        (PROGN
         (PRECMSG (LENGTH (EXPLODE (ABS (CAR (SETQ U (CDR U)))))))
         (DECIMAL2INTERNAL (CAR U) (CDR U)))))
      ((AND (PAIRP U) (EQ (CAR U) '|:RD:|)) (MKQUOTE U))
      ((AND (PAIRP U) (NOT (LISTP U)))
       (COND
        (*GETDECS
         (FORMGENTRAN1 (LIST 'DECLARE (LIST (CDR U) (CAR U))) VARS MODE))
        (T (GENTRANERR 'E U "Scalar definitions cannot be translated" NIL))))
      ((ATOM U) (MKQUOTE U))
      ((EQ (CAR U) 'EVAL)
       (COND ((EQ MODE 'ALGEBRAIC) (LIST 'AEVAL (FORM1 (CADR U) VARS MODE)))
             (T (FORM1 (CADR U) VARS MODE))))
      ((MEMQ (CAR U) '(LSETQ RSETQ LRSETQ))
       (PROG (OP LHS RHS)
         (SETQ OP (CAR U))
         (SETQ LHS (CADR U))
         (SETQ RHS (CADDR U))
         (COND
          ((AND (MEMQ OP '(LSETQ LRSETQ)) (LISTP LHS))
           (SETQ LHS
                   (CONS (CAR LHS)
                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                           (SETQ S (CDR LHS))
                           (COND ((NULL S) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (S) (LIST 'EVAL S))
                                             (CAR S))
                                            NIL)))
                          LOOPLABEL
                           (SETQ S (CDR S))
                           (COND ((NULL S) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (S) (LIST 'EVAL S)) (CAR S))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))))
         (COND ((MEMQ OP '(RSETQ LRSETQ)) (SETQ RHS (LIST 'EVAL RHS))))
         (RETURN (FORMGENTRAN1 (LIST 'SETQ LHS RHS) VARS MODE))))
      (T
       (CONS 'LIST
             (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
               (SETQ ELT U)
               (COND ((NULL ELT) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (ELT) (FORMGENTRAN1 ELT VARS MODE))
                                 (CAR ELT))
                                NIL)))
              LOOPLABEL
               (SETQ ELT (CDR ELT))
               (COND ((NULL ELT) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (ELT) (FORMGENTRAN1 ELT VARS MODE)) (CAR ELT))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL))))))) 
(PUT 'STRIP_PROCEDURE_INFO 'NUMBER-OF-ARGS 1) 
(PUT 'STRIP_PROCEDURE_INFO 'DEFINED-ON-LINE '379) 
(PUT 'STRIP_PROCEDURE_INFO 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'STRIP_PROCEDURE_INFO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STRIP_PROCEDURE_INFO (U)
    (PROG (J FORALL-RESULT FORALL-ENDPTR)
      (SETQ J U)
     STARTOVER
      (COND ((NULL J) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (J)
                 (COND
                  ((AND (NULL (ATOM J)) (EQCAR (CAR J) 'PROCEDURE_TYPE)) NIL)
                  (T (LIST J))))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ J (CDR J))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL J) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (J)
                 (COND
                  ((AND (NULL (ATOM J)) (EQCAR (CAR J) 'PROCEDURE_TYPE)) NIL)
                  (T (LIST J))))
               (CAR J)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ J (CDR J))
      (GO LOOPLABEL))) 
(PUT 'GENTRAN 'NUMBER-OF-ARGS 2) 
(PUT 'GENTRAN 'DEFINED-ON-LINE '393) 
(PUT 'GENTRAN 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENTRAN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GENTRAN (FORMS FLIST)
    (PROG (|:PRINT-PREC:|)
      (COND (FLIST (LISPEVAL (LIST 'GENTRANOUTPUSH (LIST 'QUOTE FLIST)))))
      (SETQ FORMS (PREPROC (LIST FORMS)))
      (COND
       ((GENTRANPARSE FORMS)
        (PROGN
         (SETQ FORMS (LISPCODE FORMS))
         (COND
          ((SMEMQ 'DIFFERENTIATE FORMS)
           (PROGN (LOAD-PACKAGE 'ADIFF) (SETQ FORMS (ADIFF-EVAL FORMS)))))
         (COND (*GENTRANOPT (SETQ FORMS (OPT FORMS))))
         (COND (*GENTRANSEG (SETQ FORMS (SEG FORMS))))
         (APPLY1 (OR (GET GENTRANLANG* 'FORMATTER) (GET 'FORTRAN 'FORMATTER))
                 (APPLY1
                  (OR (GET GENTRANLANG* 'CODEGEN) (GET 'FORTRAN 'CODEGEN))
                  FORMS)))))
      (COND
       (FLIST
        (PROGN
         (SETQ FLIST (OR (CAR *CURROUT*) (CONS 'LIST (CDR *CURROUT*))))
         (LISPEVAL '(GENTRANPOP '(NIL)))
         (RETURN FLIST)))
       (T (RETURN (OR (CAR *CURROUT*) (CONS 'LIST (CDR *CURROUT*)))))))) 
(PUT 'GENTRANIN 'NUMBER-OF-ARGS 2) 
(PUT 'GENTRANIN 'DEFINED-ON-LINE '420) 
(PUT 'GENTRANIN 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENTRANIN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GENTRANIN (INLIST OUTLIST)
    (PROG (ICH)
      (PROG (F)
        (SETQ F INLIST)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (COND ((PAIRP F) (GENTRANERR 'E F "Wrong Type of Arg" NIL))
                 ((AND (NOT (*FILEP* F)) (NEQ F (CAR *STDIN*)))
                  (GENTRANERR 'E F "Nonexistent Input File" NIL))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND (OUTLIST (LISPEVAL (LIST 'GENTRANOUTPUSH (MKQUOTE OUTLIST)))))
      (SETQ ICH (RDS NIL))
      (PROG (F)
        (SETQ F INLIST)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (COND ((EQUAL F (CAR *STDIN*)) (PUSHINPUTSTACK *STDIN*))
                  ((RETRIEVEINPUTFILEPAIR F)
                   (GENTRANERR 'E F "Template File Already Open for Input"
                    NIL))
                  (T (PUSHINPUTSTACK (MAKEINPUTFILEPAIR F))))
            (RDS (CDR *CURRIN*))
            (LISPAPPLY (OR (GET GENTRANLANG* 'PROCTEM) (GET 'FORTRAN 'PROCTEM))
                       NIL)
            (RDS ICH)
            (POPINPUTSTACK)))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND
       (OUTLIST
        (PROGN
         (SETQ OUTLIST (OR (CAR *CURROUT*) (CONS 'LIST (CDR *CURROUT*))))
         (LISPEVAL '(GENTRANPOP '(NIL)))
         (RETURN OUTLIST)))
       (T (RETURN (OR (CAR *CURROUT*) (CONS 'LIST (CDR *CURROUT*)))))))) 
(PUT 'GENTRANOUTPUSH 'NUMBER-OF-ARGS 1) 
(PUT 'GENTRANOUTPUSH 'DEFINED-ON-LINE '462) 
(PUT 'GENTRANOUTPUSH 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENTRANOUTPUSH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENTRANOUTPUSH (FLIST)
    (PROGN
     (COND
      ((ONEP (LENGTH (SETQ FLIST (FARGSTONAMES FLIST T))))
       (SETQ FLIST (CAR FLIST))))
     (PUSHOUTPUTSTACK
      (OR (RETRIEVEOUTPUTFILEPAIR FLIST) (MAKEOUTPUTFILEPAIR FLIST)))
     (OR (CAR *CURROUT*) (CONS 'LIST (CDR *CURROUT*))))) 
(PUT 'GENTRANSHUT 'NUMBER-OF-ARGS 1) 
(PUT 'GENTRANSHUT 'DEFINED-ON-LINE '472) 
(PUT 'GENTRANSHUT 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENTRANSHUT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENTRANSHUT (FLIST)
    (PROG (TRM)
      (SETQ FLIST (FARGSTONAMES FLIST NIL))
      (SETQ TRM
              (COND ((ONEP (LENGTH FLIST)) (EQUAL (CAR FLIST) (CAR *CURROUT*)))
                    ((CAR *CURROUT*) (COND ((MEMBER (CAR *CURROUT*) FLIST) T)))
                    (T
                     (LISPEVAL
                      (CONS 'AND
                            (PROG (F FORALL-RESULT FORALL-ENDPTR)
                              (SETQ F (CDR *CURROUT*))
                              (COND ((NULL F) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (F)
                                                  (COND ((MEMBER F FLIST) T)))
                                                (CAR F))
                                               NIL)))
                             LOOPLABEL
                              (SETQ F (CDR F))
                              (COND ((NULL F) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (F)
                                          (COND ((MEMBER F FLIST) T)))
                                        (CAR F))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))))))
      (DELETEFROMOUTPUTSTACK FLIST)
      (COND ((AND TRM (NEQ *CURROUT* *STDOUT*)) (PUSHOUTPUTSTACK *STDOUT*)))
      (RETURN (OR (CAR *CURROUT*) (CONS 'LIST (CDR *CURROUT*)))))) 
(PUT 'GENTRANPOP 'NUMBER-OF-ARGS 1) 
(PUT 'GENTRANPOP 'DEFINED-ON-LINE '489) 
(PUT 'GENTRANPOP 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENTRANPOP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENTRANPOP (FLIST)
    (PROGN
     (COND
      ((MEMBER 'ALL* FLIST)
       (PROG ()
        WHILELABEL
         (COND ((NOT (NEQ *OUTSTK* (LIST *STDOUT*))) (RETURN NIL)))
         (LISPEVAL '(GENTRANPOP '(NIL)))
         (GO WHILELABEL)))
      (T
       (PROGN
        (SETQ FLIST (FARGSTONAMES FLIST NIL))
        (COND ((ONEP (LENGTH FLIST)) (SETQ FLIST (CAR FLIST))))
        (POPOUTPUTSTACK FLIST))))
     (OR (CAR *CURROUT*) (CONS 'LIST (CDR *CURROUT*))))) 
(PUT 'GENDECS 'NUMBER-OF-ARGS 1) 
(PUT 'GENDECS 'DEFINED-ON-LINE '508) 
(PUT 'GENDECS 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENDECS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENDECS (NAME)
    (PROGN
     (COND ((EQUAL NAME 0) (SETQ NAME NIL)))
     (APPLY1 (OR (GET GENTRANLANG* 'FORMATTER) (GET 'FORTRAN 'FORMATTER))
             (APPLY1 (OR (GET GENTRANLANG* 'GENDECS) (GET 'FORTRAN 'GENDECS))
                     (SYMTABGET NAME '*DECS*)))
     (COND
      ((NULL *KEEPDECS)
       (PROGN (SYMTABREM NAME '*DECS*) (SYMTABREM NAME '*TYPE*) NIL)))
     (SYMTABREM NAME NIL)
     NIL)) 
(PUT 'GENTRANPAIRS 'NUMBER-OF-ARGS 1) 
(PUT 'GENTRANPAIRS 'DEFINED-ON-LINE '543) 
(PUT 'GENTRANPAIRS 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'GENTRANPAIRS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GENTRANPAIRS (PRS)
    (PROG (FORMATFN ASSIGNFN)
      (SETQ FORMATFN
              (OR (GET GENTRANLANG* 'FORMATTER) (GET 'FORTRAN 'FORMATTER)))
      (SETQ ASSIGNFN
              (OR (GET GENTRANLANG* 'ASSIGNER) (GET 'FORTRAN 'ASSIGNER)))
      (RETURN
       (PROG (PR)
         (SETQ PR PRS)
        LAB
         (COND ((NULL PR) (RETURN NIL)))
         ((LAMBDA (PR)
            (APPLY1 FORMATFN
                    (APPLY2 ASSIGNFN (LISPCODEEXP (CAR PR) *PERIOD)
                            (LISPCODEEXP (CDR PR) *PERIOD))))
          (CAR PR))
         (SETQ PR (CDR PR))
         (GO LAB))))) 
(PUT 'MAKEINPUTFILEPAIR 'NUMBER-OF-ARGS 1) 
(PUT 'MAKEINPUTFILEPAIR 'DEFINED-ON-LINE '583) 
(PUT 'MAKEINPUTFILEPAIR 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'MAKEINPUTFILEPAIR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKEINPUTFILEPAIR (FNAME) (CONS FNAME (OPEN (MKFIL FNAME) 'INPUT))) 
(PUT 'RETRIEVEINPUTFILEPAIR 'NUMBER-OF-ARGS 1) 
(PUT 'RETRIEVEINPUTFILEPAIR 'DEFINED-ON-LINE '586) 
(PUT 'RETRIEVEINPUTFILEPAIR 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'RETRIEVEINPUTFILEPAIR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RETRIEVEINPUTFILEPAIR (FNAME) (RETRIEVEFILEPAIR FNAME *INSTK*)) 
(PUT 'PUSHINPUTSTACK 'NUMBER-OF-ARGS 1) 
(PUT 'PUSHINPUTSTACK 'DEFINED-ON-LINE '589) 
(PUT 'PUSHINPUTSTACK 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'PUSHINPUTSTACK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PUSHINPUTSTACK (PR)
    (PROGN
     (SETQ *INSTK* (CONS PR *INSTK*))
     (SETQ *CURRIN* (CAR *INSTK*))
     *INSTK*)) 
(PUT 'POPINPUTSTACK 'NUMBER-OF-ARGS 0) 
(PUT 'POPINPUTSTACK 'DEFINED-ON-LINE '596) 
(PUT 'POPINPUTSTACK 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'POPINPUTSTACK 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE POPINPUTSTACK NIL
    (PROG (X)
      (SETQ X *CURRIN*)
      (COND ((CDR *CURRIN*) (CLOSE (CDR *CURRIN*))))
      (SETQ *INSTK* (OR (CDR *INSTK*) (LIST *STDIN*)))
      (SETQ *CURRIN* (CAR *INSTK*))
      (RETURN X))) 
(PUT 'MAKEOUTPUTFILEPAIR 'NUMBER-OF-ARGS 1) 
(PUT 'MAKEOUTPUTFILEPAIR 'DEFINED-ON-LINE '608) 
(PUT 'MAKEOUTPUTFILEPAIR 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'MAKEOUTPUTFILEPAIR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKEOUTPUTFILEPAIR (F)
    (COND ((ATOM F) (CONS F (OPEN (MKFIL F) 'OUTPUT)))
          (T
           (ACONC
            (CONS (CONS NIL F)
                  (PROG (FN FORALL-RESULT FORALL-ENDPTR)
                    (SETQ FN F)
                   STARTOVER
                    (COND ((NULL FN) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            ((LAMBDA (FN)
                               (COND
                                ((NOT (RETRIEVEOUTPUTFILEPAIR FN))
                                 (LIST (MAKEOUTPUTFILEPAIR FN)))))
                             (CAR FN)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                    (SETQ FN (CDR FN))
                    (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                   LOOPLABEL
                    (COND ((NULL FN) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            ((LAMBDA (FN)
                               (COND
                                ((NOT (RETRIEVEOUTPUTFILEPAIR FN))
                                 (LIST (MAKEOUTPUTFILEPAIR FN)))))
                             (CAR FN)))
                    (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                    (SETQ FN (CDR FN))
                    (GO LOOPLABEL)))
            (CONS NIL NIL))))) 
(PUT 'RETRIEVEOUTPUTFILEPAIR 'NUMBER-OF-ARGS 1) 
(PUT 'RETRIEVEOUTPUTFILEPAIR 'DEFINED-ON-LINE '618) 
(PUT 'RETRIEVEOUTPUTFILEPAIR 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'RETRIEVEOUTPUTFILEPAIR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RETRIEVEOUTPUTFILEPAIR (F)
    (COND ((ATOM F) (RETRIEVEFILEPAIR F *OUTSTK*))
          (T (RETRIEVEPFILEPAIR F *OUTSTK*)))) 
(PUT 'PUSHOUTPUTSTACK 'NUMBER-OF-ARGS 1) 
(PUT 'PUSHOUTPUTSTACK 'DEFINED-ON-LINE '623) 
(PUT 'PUSHOUTPUTSTACK 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'PUSHOUTPUTSTACK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PUSHOUTPUTSTACK (PR)
    (PROGN
     (SETQ *OUTSTK*
             (COND ((ATOM (CDR PR)) (CONS PR *OUTSTK*))
                   (T (APPEND PR *OUTSTK*))))
     (SETQ *CURROUT* (CAR *OUTSTK*))
     (SETQ *OUTCHANL*
             (COND ((CAR *CURROUT*) (LIST (CDR *CURROUT*)))
                   (T
                    (PROG (F FORALL-RESULT FORALL-ENDPTR)
                      (SETQ F (CDR *CURROUT*))
                      (COND ((NULL F) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (F)
                                          (CDR (RETRIEVEOUTPUTFILEPAIR F)))
                                        (CAR F))
                                       NIL)))
                     LOOPLABEL
                      (SETQ F (CDR F))
                      (COND ((NULL F) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (F) (CDR (RETRIEVEOUTPUTFILEPAIR F)))
                                (CAR F))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))))
     *OUTSTK*)) 
(PUT 'POPOUTPUTSTACK 'NUMBER-OF-ARGS 1) 
(PUT 'POPOUTPUTSTACK 'DEFINED-ON-LINE '636) 
(PUT 'POPOUTPUTSTACK 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'POPOUTPUTSTACK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE POPOUTPUTSTACK (F)
    (PROG (PR S)
      (COND
       ((ATOM F)
        (PROGN
         (SETQ PR (RETRIEVEOUTPUTFILEPAIR F))
         (PROG ()
          WHILELABEL
           (COND ((NOT (AND *OUTSTK* (NEQ (CAR *OUTSTK*) PR))) (RETURN NIL)))
           (COND
            ((CAAR *OUTSTK*)
             (PROGN
              (SETQ S (ACONC S (CAR *OUTSTK*)))
              (SETQ *OUTSTK* (CDR *OUTSTK*))))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND ((NOT (NEQ (CAR *OUTSTK*) (CONS NIL NIL))) (RETURN NIL)))
                (PROGN
                 (SETQ S (ACONC S (CAR *OUTSTK*)))
                 (SETQ *OUTSTK* (CDR *OUTSTK*)))
                (GO WHILELABEL))
              (SETQ S (ACONC S (CAR *OUTSTK*)))
              (SETQ *OUTSTK* (CDR *OUTSTK*)))))
           (GO WHILELABEL))
         (COND (*OUTSTK* (SETQ S (APPEND S (CDR *OUTSTK*)))))
         (SETQ *OUTSTK* S)
         (COND ((NOT (RETRIEVEOUTPUTFILEPAIR F)) (CLOSE (CDR PR))))))
       (T
        (PROGN
         (SETQ PR
                 (PROG (FN FORALL-RESULT FORALL-ENDPTR)
                   (SETQ FN F)
                   (COND ((NULL FN) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (FN) (RETRIEVEOUTPUTFILEPAIR FN))
                                     (CAR FN))
                                    NIL)))
                  LOOPLABEL
                   (SETQ FN (CDR FN))
                   (COND ((NULL FN) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (FN) (RETRIEVEOUTPUTFILEPAIR FN))
                             (CAR FN))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (AND *OUTSTK* (NOT (FILELISTEQUIVP (CDAR *OUTSTK*) F))))
             (RETURN NIL)))
           (COND
            ((CAAR *OUTSTK*)
             (PROGN
              (SETQ S (ACONC S (CAR *OUTSTK*)))
              (SETQ *OUTSTK* (CDR *OUTSTK*))))
            (T
             (PROGN
              (PROG ()
               WHILELABEL
                (COND ((NOT (NEQ (CAR *OUTSTK*) (CONS NIL NIL))) (RETURN NIL)))
                (PROGN
                 (SETQ S (ACONC S (CAR *OUTSTK*)))
                 (SETQ *OUTSTK* (CDR *OUTSTK*)))
                (GO WHILELABEL))
              (SETQ S (ACONC S (CAR *OUTSTK*)))
              (SETQ *OUTSTK* (CDR *OUTSTK*)))))
           (GO WHILELABEL))
         (COND
          (*OUTSTK*
           (PROGN
            (PROG ()
             WHILELABEL
              (COND ((NOT (NEQ (CAR *OUTSTK*) (CONS NIL NIL))) (RETURN NIL)))
              (SETQ *OUTSTK* (CDR *OUTSTK*))
              (GO WHILELABEL))
            (SETQ S (APPEND S (CDR *OUTSTK*))))))
         (SETQ *OUTSTK* S)
         (PROG (FN)
           (SETQ FN F)
          LAB
           (COND ((NULL FN) (RETURN NIL)))
           ((LAMBDA (FN) (SETQ PR (DELETE (RETRIEVEOUTPUTFILEPAIR FN) PR)))
            (CAR FN))
           (SETQ FN (CDR FN))
           (GO LAB))
         (PROG (P)
           (SETQ P PR)
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P) (CLOSE (CDR P))) (CAR P))
           (SETQ P (CDR P))
           (GO LAB)))))
      (SETQ *OUTSTK* (OR *OUTSTK* (LIST *STDOUT*)))
      (SETQ *CURROUT* (CAR *OUTSTK*))
      (SETQ *OUTCHANL*
              (COND ((CAR *CURROUT*) (LIST (CDR *CURROUT*)))
                    (T
                     (PROG (FN FORALL-RESULT FORALL-ENDPTR)
                       (SETQ FN (CDR *CURROUT*))
                       (COND ((NULL FN) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (FN)
                                           (CDR (RETRIEVEOUTPUTFILEPAIR FN)))
                                         (CAR FN))
                                        NIL)))
                      LOOPLABEL
                       (SETQ FN (CDR FN))
                       (COND ((NULL FN) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (FN)
                                   (CDR (RETRIEVEOUTPUTFILEPAIR FN)))
                                 (CAR FN))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (RETURN F))) 
(PUT 'DELETEFROMOUTPUTSTACK 'NUMBER-OF-ARGS 1) 
(PUT 'DELETEFROMOUTPUTSTACK 'DEFINED-ON-LINE '692) 
(PUT 'DELETEFROMOUTPUTSTACK 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'DELETEFROMOUTPUTSTACK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DELETEFROMOUTPUTSTACK (F)
    (PROG (S PR)
      (COND
       ((ATOM F)
        (PROGN
         (SETQ PR (RETRIEVEOUTPUTFILEPAIR F))
         (PROG ()
          WHILELABEL
           (COND ((NOT (RETRIEVEOUTPUTFILEPAIR F)) (RETURN NIL)))
           (SETQ *OUTSTK* (DELETE PR *OUTSTK*))
           (GO WHILELABEL))
         (CLOSE (CDR PR))
         (PROG (PR)
           (SETQ PR *OUTSTK*)
          LAB
           (COND ((NULL PR) (RETURN NIL)))
           ((LAMBDA (PR)
              (COND
               ((AND (LISTP (CDR PR)) (PAIRP (CDR PR)) (MEMBER F (CDR PR)))
                (RPLACD PR (DELETE F (CDR PR))))))
            (CAR PR))
           (SETQ PR (CDR PR))
           (GO LAB))))
       (T
        (PROGN
         (PROG (FN)
           (SETQ FN F)
          LAB
           (COND ((NULL FN) (RETURN NIL)))
           ((LAMBDA (FN) (DELETEFROMOUTPUTSTACK FN)) (CAR FN))
           (SETQ FN (CDR FN))
           (GO LAB))
         (PROG (FN)
           (SETQ FN F)
          LAB
           (COND ((NULL FN) (RETURN NIL)))
           ((LAMBDA (FN)
              (PROG (PR)
                (SETQ PR *OUTSTK*)
               LAB
                (COND ((NULL PR) (RETURN NIL)))
                ((LAMBDA (PR)
                   (COND
                    ((AND (PAIRP (CDR PR)) (MEMBER FN (CDR PR)))
                     (RPLACD PR (DELETE FN (CDR PR))))))
                 (CAR PR))
                (SETQ PR (CDR PR))
                (GO LAB)))
            (CAR FN))
           (SETQ FN (CDR FN))
           (GO LAB)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT *OUTSTK*) (RETURN NIL)))
        (COND
         ((AND (CAAR *OUTSTK*) (NEQ (CAAR *OUTSTK*) 'T))
          (PROGN
           (SETQ S (ACONC S (CAR *OUTSTK*)))
           (SETQ *OUTSTK* (CDR *OUTSTK*))))
         ((AND (CDAR *OUTSTK*) (NEQ (CDAR *OUTSTK*) '(T)))
          (PROGN
           (PROG ()
            WHILELABEL
             (COND ((NOT (NEQ (CAR *OUTSTK*) (CONS NIL NIL))) (RETURN NIL)))
             (PROGN
              (SETQ S (ACONC S (CAR *OUTSTK*)))
              (SETQ *OUTSTK* (CDR *OUTSTK*)))
             (GO WHILELABEL))
           (SETQ S (ACONC S (CAR *OUTSTK*)))
           (SETQ *OUTSTK* (CDR *OUTSTK*))))
         ((CDR *OUTSTK*) (SETQ *OUTSTK* (CDDR *OUTSTK*)))
         (T (SETQ *OUTSTK* NIL)))
        (GO WHILELABEL))
      (SETQ *OUTSTK* (OR S (LIST *STDOUT*)))
      (SETQ *CURROUT* (CAR *OUTSTK*))
      (SETQ *OUTCHANL*
              (COND ((CAR *CURROUT*) (LIST (CDR *CURROUT*)))
                    (T
                     (PROG (FN FORALL-RESULT FORALL-ENDPTR)
                       (SETQ FN (CDR *CURROUT*))
                       (COND ((NULL FN) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (FN)
                                           (CDR (RETRIEVEOUTPUTFILEPAIR FN)))
                                         (CAR FN))
                                        NIL)))
                      LOOPLABEL
                       (SETQ FN (CDR FN))
                       (COND ((NULL FN) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (FN)
                                   (CDR (RETRIEVEOUTPUTFILEPAIR FN)))
                                 (CAR FN))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (RETURN F))) 
(PUT 'RETRIEVEFILEPAIR 'NUMBER-OF-ARGS 2) 
(PUT 'RETRIEVEFILEPAIR 'DEFINED-ON-LINE '743) 
(PUT 'RETRIEVEFILEPAIR 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'RETRIEVEFILEPAIR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RETRIEVEFILEPAIR (FNAME STK)
    (COND ((NULL STK) NIL)
          ((AND (CAAR STK) (EQUAL (MKFIL FNAME) (MKFIL (CAAR STK)))) (CAR STK))
          (T (RETRIEVEFILEPAIR FNAME (CDR STK))))) 
(PUT 'RETRIEVEPFILEPAIR 'NUMBER-OF-ARGS 2) 
(PUT 'RETRIEVEPFILEPAIR 'DEFINED-ON-LINE '751) 
(PUT 'RETRIEVEPFILEPAIR 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'RETRIEVEPFILEPAIR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RETRIEVEPFILEPAIR (F STK)
    (COND ((NULL STK) NIL)
          ((AND (NULL (CAAR STK)) (FILELISTEQUIVP F (CDAR STK)))
           (LIST (CAR STK) (CONS NIL NIL)))
          (T (RETRIEVEPFILEPAIR F (CDR STK))))) 
(PUT 'FILELISTEQUIVP 'NUMBER-OF-ARGS 2) 
(PUT 'FILELISTEQUIVP 'DEFINED-ON-LINE '759) 
(PUT 'FILELISTEQUIVP 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'FILELISTEQUIVP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FILELISTEQUIVP (F1 F2)
    (COND
     ((AND (PAIRP F1) (PAIRP F2))
      (PROGN
       (SETQ F1
               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                 (SETQ F F1)
                 (COND ((NULL F) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (F) (MKFIL F)) (CAR F)) NIL)))
                LOOPLABEL
                 (SETQ F (CDR F))
                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (MKFIL F)) (CAR F)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))
       (SETQ F2
               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                 (SETQ F F2)
                 (COND ((NULL F) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (F) (MKFIL F)) (CAR F)) NIL)))
                LOOPLABEL
                 (SETQ F (CDR F))
                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (MKFIL F)) (CAR F)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL)))
       (PROG ()
        WHILELABEL
         (COND ((NOT (MEMBER (CAR F1) F2)) (RETURN NIL)))
         (PROGN (SETQ F2 (DELETE (CAR F1) F2)) (SETQ F1 (CDR F1)))
         (GO WHILELABEL))
       (AND (NULL F1) (NULL F2)))))) 
(PUT '*FILEP* 'NUMBER-OF-ARGS 1) 
(PUT '*FILEP* 'DEFINED-ON-LINE '775) 
(PUT '*FILEP* 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT '*FILEP* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *FILEP* (F)
    (NOT
     (ERRORP
      (ERRORSET (LIST 'CLOSE (LIST 'OPEN (LIST 'MKFIL (MKQUOTE F)) ''INPUT))
                NIL NIL)))) 
(PUT 'ENDOFSTMTP 'NUMBER-OF-ARGS 0) 
(PUT 'ENDOFSTMTP 'DEFINED-ON-LINE '785) 
(PUT 'ENDOFSTMTP 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'ENDOFSTMTP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ENDOFSTMTP NIL (COND ((MEMBER CURSYM* '(*SEMICOL* *ENDGROUP* END)) T))) 
(PUT 'FARGSTONAMES 'NUMBER-OF-ARGS 2) 
(PUT 'FARGSTONAMES 'DEFINED-ON-LINE '788) 
(PUT 'FARGSTONAMES 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'FARGSTONAMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FARGSTONAMES (FARGS OPENP)
    (PROG (NAMES)
      (SETQ FARGS
              (PROG (A FORALL-RESULT FORALL-ENDPTR)
                (SETQ A FARGS)
               STARTOVER
                (COND ((NULL A) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (A)
                           (COND
                            ((MEMQ A '(NIL 0))
                             (COND ((CAR *CURROUT*) (LIST (CAR *CURROUT*)))
                                   (T (CDR *CURROUT*))))
                            ((EQ A 'T) (LIST (CAR *STDOUT*)))
                            ((EQ A 'ALL*)
                             (PROG (FP FORALL-RESULT FORALL-ENDPTR)
                               (SETQ FP *OUTSTK*)
                              STARTOVER
                               (COND ((NULL FP) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       ((LAMBDA (FP)
                                          (COND
                                           ((AND (CAR FP)
                                                 (NOT (EQUAL FP *STDOUT*)))
                                            (LIST (CAR FP)))))
                                        (CAR FP)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                               (SETQ FP (CDR FP))
                               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                              LOOPLABEL
                               (COND ((NULL FP) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       ((LAMBDA (FP)
                                          (COND
                                           ((AND (CAR FP)
                                                 (NOT (EQUAL FP *STDOUT*)))
                                            (LIST (CAR FP)))))
                                        (CAR FP)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                               (SETQ FP (CDR FP))
                               (GO LOOPLABEL)))
                            ((ATOM A)
                             (COND
                              (OPENP
                               (PROGN
                                (COND
                                 ((AND (NULL (GETD 'BPSMOVE)) (*FILEP* A)
                                       (NULL (ASSOC A *OUTSTK*)))
                                  (GENTRANERR 'W A "OUTPUT FILE ALREADY EXISTS"
                                   "CONTINUE?")))
                                (LIST A)))
                              ((RETRIEVEOUTPUTFILEPAIR A) (LIST A))
                              (T
                               (GENTRANERR 'W A "File not Open for Output"
                                NIL))))
                            (T (GENTRANERR 'E A "WRONG TYPE OF ARG" NIL))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ A (CDR A))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL A) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (A)
                           (COND
                            ((MEMQ A '(NIL 0))
                             (COND ((CAR *CURROUT*) (LIST (CAR *CURROUT*)))
                                   (T (CDR *CURROUT*))))
                            ((EQ A 'T) (LIST (CAR *STDOUT*)))
                            ((EQ A 'ALL*)
                             (PROG (FP FORALL-RESULT FORALL-ENDPTR)
                               (SETQ FP *OUTSTK*)
                              STARTOVER
                               (COND ((NULL FP) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       ((LAMBDA (FP)
                                          (COND
                                           ((AND (CAR FP)
                                                 (NOT (EQUAL FP *STDOUT*)))
                                            (LIST (CAR FP)))))
                                        (CAR FP)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                               (SETQ FP (CDR FP))
                               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                              LOOPLABEL
                               (COND ((NULL FP) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       ((LAMBDA (FP)
                                          (COND
                                           ((AND (CAR FP)
                                                 (NOT (EQUAL FP *STDOUT*)))
                                            (LIST (CAR FP)))))
                                        (CAR FP)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                               (SETQ FP (CDR FP))
                               (GO LOOPLABEL)))
                            ((ATOM A)
                             (COND
                              (OPENP
                               (PROGN
                                (COND
                                 ((AND (NULL (GETD 'BPSMOVE)) (*FILEP* A)
                                       (NULL (ASSOC A *OUTSTK*)))
                                  (GENTRANERR 'W A "OUTPUT FILE ALREADY EXISTS"
                                   "CONTINUE?")))
                                (LIST A)))
                              ((RETRIEVEOUTPUTFILEPAIR A) (LIST A))
                              (T
                               (GENTRANERR 'W A "File not Open for Output"
                                NIL))))
                            (T (GENTRANERR 'E A "WRONG TYPE OF ARG" NIL))))
                         (CAR A)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ A (CDR A))
                (GO LOOPLABEL)))
      (PROG ()
       REPEATLABEL
        (COND
         ((NOT (MEMBER (CAR FARGS) NAMES))
          (SETQ NAMES (APPEND NAMES (LIST (CAR FARGS))))))
        (COND ((NOT (NULL (SETQ FARGS (CDR FARGS)))) (GO REPEATLABEL))))
      (RETURN NAMES))) 
(PUT 'READFARGS 'NUMBER-OF-ARGS 0) 
(PUT 'READFARGS 'DEFINED-ON-LINE '828) 
(PUT 'READFARGS 'DEFINED-IN-FILE 'GENTRAN/INTRFC.RED) 
(PUT 'READFARGS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READFARGS NIL
    (PROG (F)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (ENDOFSTMTP))) (RETURN NIL)))
        (SETQ F (APPEND F (LIST (XREAD T))))
        (GO WHILELABEL))
      (RETURN (OR F (LIST NIL))))) 
(ENDMODULE) 