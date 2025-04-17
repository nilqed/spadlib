(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LSPC)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(FLUID '(*DOUBLE *GENDECS)) 
(SWITCH (LIST 'GENDECS)) 
(GLOBAL '(CLINELEN* MINCLINELEN* *CCURRIND* CCURRIND* TABLEN*)) 
(SHARE (LIST 'CLINELEN* 'MINCLINELEN* 'CCURRIND* 'TABLEN*)) 
(SETQ CCURRIND* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 0)) 
(SETQ CLINELEN* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 80)) 
(SETQ MINCLINELEN* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 40)) 
(SETQ *CCURRIND* 0) 
(GLOBAL '(DEFTYPE* *C-FUNCTIONS*)) 
(GLOBAL '(*POSN* |$#|)) 
(SETQ *C-FUNCTIONS*
        '(SIN COS TAN ASIN ACOS ATAN ATAN2 SINH COSH TANH ASINH ACOSH ATANH
              SINCOS SINPI COSPI TANPI ASINPI ACOSPI ATANPI EXP EXPM1 EXP2
              EXP10 LOG LOG1P LOG2 LOG10 POW COMPOUND ANNUITY ABS FABS FMOD
              SQRT CBRT)) 
(FLAG '(ABS) '*INT-ARGS*) 
(PUT 'C 'FORMATTER 'FORMATC) 
(PUT 'C 'CODEGEN 'CCODE) 
(PUT 'C 'PROCTEM 'PROCCTEM) 
(PUT 'C 'GENDECS 'CDECS) 
(PUT 'C 'ASSIGNER 'MKFCASSIGN) 
(PUT 'C 'BOOLEAN-TYPE 'INT) 
(PUT 'CCODE 'NUMBER-OF-ARGS 1) 
(PUT 'CCODE 'DEFINED-ON-LINE '73) 
(PUT 'CCODE 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CCODE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CCODE (FORMS)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F FORMS)
     STARTOVER
      (COND ((NULL F) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (F)
                 (COND ((ATOM F) (CEXP F))
                       ((MEMQ (CAR F) '(|:RD:| |:CR:| |:CRN:| |:GI:|))
                        (CEXP F))
                       ((OR (LISPSTMTP F) (LISPSTMTGPP F))
                        (COND
                         (*GENDECS
                          (PROG (R)
                            (SETQ R
                                    (APPEND (CDECS (SYMTABGET '*MAIN* '*DECS*))
                                            (CSTMT F)))
                            (SYMTABREM '*MAIN* '*DECS*)
                            (RETURN R)))
                         (T (CSTMT F))))
                       ((LISPDEFP F) (CPROC F)) (T (CEXP F))))
               (CAR F)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ F (CDR F))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (F)
                 (COND ((ATOM F) (CEXP F))
                       ((MEMQ (CAR F) '(|:RD:| |:CR:| |:CRN:| |:GI:|))
                        (CEXP F))
                       ((OR (LISPSTMTP F) (LISPSTMTGPP F))
                        (COND
                         (*GENDECS
                          (PROG (R)
                            (SETQ R
                                    (APPEND (CDECS (SYMTABGET '*MAIN* '*DECS*))
                                            (CSTMT F)))
                            (SYMTABREM '*MAIN* '*DECS*)
                            (RETURN R)))
                         (T (CSTMT F))))
                       ((LISPDEFP F) (CPROC F)) (T (CEXP F))))
               (CAR F)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ F (CDR F))
      (GO LOOPLABEL))) 
(PUT 'CPROC 'NUMBER-OF-ARGS 1) 
(PUT 'CPROC 'DEFINED-ON-LINE '99) 
(PUT 'CPROC 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CPROC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CPROC (DEFF)
    (PROG (TYPE NAME PARAMS PARAMTYPES VARTYPES BODY R)
      (SETQ NAME (CADR DEFF))
      (COND
       ((AND (ONEP (LENGTH (SETQ BODY (CDDDR DEFF)))) (LISPSTMTGPP (CAR BODY)))
        (PROGN
         (SETQ BODY (CDAR BODY))
         (COND ((NULL (CAR BODY)) (SETQ BODY (CDR BODY)))))))
      (COND
       ((SETQ TYPE (SYMTABGET NAME NAME))
        (PROGN
         (SETQ TYPE (CADR TYPE))
         (COND ((EQUAL TYPE 'REAL) (SETQ TYPE 'FLOAT))
               ((EQUAL TYPE 'INTEGER) (SETQ TYPE 'INT)))
         (COND
          (*DOUBLE
           (COND ((EQUAL TYPE 'FLOAT) (SETQ TYPE 'DOUBLE))
                 ((EQUAL TYPE 'INT) (SETQ TYPE 'LONG)))))
         (SYMTABREM NAME NAME))))
      (SETQ PARAMS (OR (SYMTABGET NAME '*PARAMS*) (CADDR DEFF)))
      (SYMTABREM NAME '*PARAMS*)
      (PROG (DEC)
        (SETQ DEC (SYMTABGET NAME '*DECS*))
       LAB
        (COND ((NULL DEC) (RETURN NIL)))
        ((LAMBDA (DEC)
           (COND
            ((MEMQ (CAR DEC) PARAMS)
             (SETQ PARAMTYPES (APPEND PARAMTYPES (LIST DEC))))
            (T (SETQ VARTYPES (APPEND VARTYPES (LIST DEC))))))
         (CAR DEC))
        (SETQ DEC (CDR DEC))
        (GO LAB))
      (SETQ R
              (APPEND
               (APPEND (MKFCPROCDEC TYPE NAME PARAMS) (CDECS PARAMTYPES))
               (MKFCBEGINGP)))
      (INDENTCLEVEL (PLUS 1))
      (COND (*GENDECS (SETQ R (APPEND R (CDECS VARTYPES)))))
      (SETQ R
              (APPEND R
                      (PROG (S FORALL-RESULT FORALL-ENDPTR)
                        (SETQ S BODY)
                       STARTOVER
                        (COND ((NULL S) (RETURN NIL)))
                        (SETQ FORALL-RESULT ((LAMBDA (S) (CSTMT S)) (CAR S)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ S (CDR S))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL S) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR ((LAMBDA (S) (CSTMT S)) (CAR S)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ S (CDR S))
                        (GO LOOPLABEL))))
      (INDENTCLEVEL (MINUS 1))
      (SETQ R (APPEND R (MKFCENDGP)))
      (COND (*GENDECS (PROGN (SYMTABREM NAME NIL) (SYMTABREM NAME '*DECS*))))
      (RETURN R))) 
(PUT 'CDECS 'NUMBER-OF-ARGS 1) 
(PUT 'CDECS 'DEFINED-ON-LINE '148) 
(PUT 'CDECS 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CDECS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CDECS (DECS)
    (PROG (TL FORALL-RESULT FORALL-ENDPTR)
      (SETQ TL (FORMTYPELISTS DECS))
     STARTOVER
      (COND ((NULL TL) (RETURN NIL)))
      (SETQ FORALL-RESULT ((LAMBDA (TL) (MKFCDEC (CAR TL) (CDR TL))) (CAR TL)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ TL (CDR TL))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL TL) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (TL) (MKFCDEC (CAR TL) (CDR TL))) (CAR TL)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ TL (CDR TL))
      (GO LOOPLABEL))) 
(PUT 'CEXP 'NUMBER-OF-ARGS 1) 
(PUT 'CEXP 'DEFINED-ON-LINE '156) 
(PUT 'CEXP 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CEXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CEXP (EXP) (CEXP1 EXP 0)) 
(PUT 'CEXP1 'NUMBER-OF-ARGS 2) 
(PUT 'CEXP1 'DEFINED-ON-LINE '159) 
(PUT 'CEXP1 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CEXP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CEXP1 (EXP WTIN)
    (COND ((ATOM EXP) (LIST (CNAME EXP)))
          ((ONEP (LENGTH EXP)) (APPEND (CNAME EXP) (INSERTPARENS NIL)))
          ((EQ (CAR EXP) 'EXPT)
           (COND
            ((EQUAL (CADDR EXP) 2)
             (CEXP1 (LIST 'TIMES (CADR EXP) (CADR EXP)) WTIN))
            ((EQUAL (CADDR EXP) 3)
             (CEXP1 (LIST 'TIMES (CADR EXP) (CADR EXP) (CADR EXP)) WTIN))
            ((EQUAL (CADDR EXP) 4)
             (CEXP1 (LIST 'TIMES (CADR EXP) (CADR EXP) (CADR EXP) (CADR EXP))
              WTIN))
            ((EQUAL (CADDR EXP) '(QUOTIENT 1 2))
             (CEXP1 (LIST 'SQRT (CADR EXP)) WTIN))
            (T (CEXP1 (CONS 'POW (CDR EXP)) WTIN))))
          ((OPTYPE (CAR EXP))
           (PROG (WT OP RES)
             (SETQ WT (CPRECEDENCE (CAR EXP)))
             (SETQ OP (COP (CAR EXP)))
             (SETQ EXP (CDR EXP))
             (COND
              ((ONEP (LENGTH EXP)) (SETQ RES (CONS OP (CEXP1 (CAR EXP) WT))))
              (T
               (PROGN
                (SETQ RES (CEXP1 (CAR EXP) WT))
                (COND
                 ((EQ OP '+)
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (SETQ EXP (CDR EXP))) (RETURN NIL)))
                    (PROGN
                     (COND
                      ((OR (ATOM (CAR EXP)) (NEQ (CAAR EXP) 'MINUS))
                       (SETQ RES (APPEND RES (LIST OP)))))
                     (SETQ RES (APPEND RES (CEXP1 (CAR EXP) WT))))
                    (GO WHILELABEL)))
                 (T
                  (PROG ()
                   WHILELABEL
                    (COND ((NOT (SETQ EXP (CDR EXP))) (RETURN NIL)))
                    (SETQ RES
                            (APPEND (APPEND RES (LIST OP))
                                    (CEXP1 (CAR EXP) WT)))
                    (GO WHILELABEL)))))))
             (COND ((GEQ WTIN WT) (SETQ RES (INSERTPARENS RES))))
             (RETURN RES)))
          ((EQ (CAR EXP) 'LITERAL) (CLITERAL EXP))
          ((EQ (CAR EXP) 'RANGE)
           (COND ((EQUAL (CADR EXP) 0) (CEXP (CADDR EXP)))
                 (T
                  (GENTRANERR 'E EXP "C does not support non-zero lower bounds"
                   NIL))))
          ((EQ (CAR EXP) '|:RD:|)
           (COND ((ATOM (CDR EXP)) (LIST (CDR EXP)))
                 (T
                  (PROG (MT DOTPOS |:LOWER-SCI:| |:UPPER-SCI:|)
                    (SETQ DOTPOS 0)
                    (SETQ |:LOWER-SCI:| 0)
                    (SETQ |:UPPER-SCI:| 0)
                    (SETQ MT (|RD:EXPLODE| EXP))
                    (SETQ EXP (CAR MT))
                    (SETQ MT (PLUS (CADR MT) (DIFFERENCE (CADDR MT) 1)))
                    (SETQ EXP
                            (APPEND (LIST 'LITERAL (CAR EXP) '|.|) (CDR EXP)))
                    (COND
                     ((NULL (EQUAL MT 0))
                      (SETQ EXP (APPEND EXP (LIST 'E MT)))))
                    (RETURN (CLITERAL EXP))))))
          ((MEMQ (CAR EXP) '(|:CR:| |:CRN:| |:GI:|))
           (GENTRANERR 'E EXP "C doesn't support complex data type" NIL))
          ((ARRAYELTP EXP)
           (CONS (CNAME (CAR EXP))
                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                   (SETQ S (CDR EXP))
                  STARTOVER
                   (COND ((NULL S) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (S) (INSERTBRACKETS (CEXP1 S 0))) (CAR S)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ S (CDR S))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL S) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (S) (INSERTBRACKETS (CEXP1 S 0))) (CAR S)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ S (CDR S))
                   (GO LOOPLABEL))))
          ((MEMQ (CAR EXP) *C-FUNCTIONS*)
           (PROG (OP RES DBLP)
             (SETQ DBLP (NOT (GET (CAR EXP) '*INT-ARGS*)))
             (SETQ OP (CNAME (CAR EXP)))
             (SETQ RES (CONS '|(| (LIST OP)))
             (PROG ()
              WHILELABEL
               (COND ((NOT (SETQ EXP (CDR EXP))) (RETURN NIL)))
               (PROGN
                (SETQ OP (CEXP1 (CAR EXP) 0))
                (COND
                 ((AND DBLP (NOT (OR (IS-C-FLOAT OP) (IS-C-FLOAT (CAR EXP)))))
                  (SETQ OP
                          (COND
                           ((FIXP (CAR OP)) (CONS (FLOAT (CAR OP)) (CDR OP)))
                           (T
                            (APPEND (LIST '|(| 'DOUBLE '|)| '|(|)
                                    (APPEND OP (LIST '|)|))))))))
                (SETQ RES
                        (COND
                         ((CDR EXP) (APPEND (CONS '|,| (REVERSIP OP)) RES))
                         (T (APPEND (REVERSIP OP) RES))))
                NIL)
               (GO WHILELABEL))
             (RETURN (REVERSIP (CONS '|)| RES)))))
          ((CFUNCTCALLP EXP)
           (PROG (OP RES)
             (SETQ OP (CNAME (CAR EXP)))
             (SETQ EXP (CDR EXP))
             (SETQ RES (CONS '|(| (CEXP1 (CAR EXP) 0)))
             (PROG ()
              WHILELABEL
               (COND ((NOT (SETQ EXP (CDR EXP))) (RETURN NIL)))
               (SETQ RES (APPEND RES (CONS '|,| (CEXP1 (CAR EXP) 0))))
               (GO WHILELABEL))
             (RETURN (CONS OP (APPEND RES (LIST '|)|))))))
          (T
           (PROG (OP RES)
             (SETQ OP (CNAME (CAR EXP)))
             (SETQ EXP (CDR EXP))
             (SETQ RES (APPEND (CONS '[ (CEXP1 (CAR EXP) 0)) (LIST '])))
             (PROG ()
              WHILELABEL
               (COND ((NOT (SETQ EXP (CDR EXP))) (RETURN NIL)))
               (SETQ RES
                       (APPEND RES
                               (APPEND (CONS '[ (CEXP1 (CAR EXP) 0))
                                       (LIST ']))))
               (GO WHILELABEL))
             (RETURN (CONS OP RES)))))) 
(PUT 'STRING2ID 'NUMBER-OF-ARGS 1) 
(PUT 'STRING2ID 'DEFINED-ON-LINE '275) 
(PUT 'STRING2ID 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'STRING2ID 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE STRING2ID (STR)
    (INTERN (COMPRESS (REVERSIP (CDR (REVERSIP (CDR (EXPLODE STR)))))))) 
(PUT 'IS-C-FLOAT 'NUMBER-OF-ARGS 1) 
(PUT 'IS-C-FLOAT 'DEFINED-ON-LINE '278) 
(PUT 'IS-C-FLOAT 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'IS-C-FLOAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IS-C-FLOAT (U)
    (OR (FLOATP U) (AND (IDP U) (DECLARED-AS-FLOAT U))
        (AND (PAIRP U)
             (OR (EQ (CAR U) '|:RD:|)
                 (AND (STRINGP (CAR U))
                      (MEMQ (STRING2ID (CAR U)) *C-FUNCTIONS*)
                      (NOT (FLAGP (STRING2ID (CAR U)) '*INT-ARGS*)))
                 (DECLARED-AS-FLOAT (CAR U)))))) 
(PUT 'CFUNCTCALLP 'NUMBER-OF-ARGS 1) 
(PUT 'CFUNCTCALLP 'DEFINED-ON-LINE '288) 
(PUT 'CFUNCTCALLP 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CFUNCTCALLP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFUNCTCALLP (EXP) (SYMTABGET (CAR EXP) '*TYPE*)) 
(PUT 'COP 'NUMBER-OF-ARGS 1) 
(PUT 'COP 'DEFINED-ON-LINE '292) 
(PUT 'COP 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'COP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COP (OP) (OR (GET OP '*COP*) OP)) 
(PUT 'OR '*COP* '|\|\||) 
(PUT 'AND '*COP* '&&) 
(PUT 'NOT '*COP* '!) 
(PUT 'EQUAL '*COP* '==) 
(PUT 'NEQ '*COP* '!=) 
(PUT 'GREATERP '*COP* '>) 
(PUT 'GEQ '*COP* '>=) 
(PUT 'LESSP '*COP* '<) 
(PUT 'LEQ '*COP* '<=) 
(PUT 'PLUS '*COP* '+) 
(PUT 'TIMES '*COP* '*) 
(PUT 'QUOTIENT '*COP* '/) 
(PUT 'MINUS '*COP* '-) 
(PUT 'CNAME 'NUMBER-OF-ARGS 1) 
(PUT 'CNAME 'DEFINED-ON-LINE '309) 
(PUT 'CNAME 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CNAME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CNAME (A)
    (COND ((STRINGP A) (STRINGTOATOM A))
          ((MEMQ A *C-FUNCTIONS*) (STRING-DOWNCASE A))
          (T (OR (GET A '*CNAME*) A)))) 
(PUT 'CPRECEDENCE 'NUMBER-OF-ARGS 1) 
(PUT 'CPRECEDENCE 'DEFINED-ON-LINE '317) 
(PUT 'CPRECEDENCE 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CPRECEDENCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CPRECEDENCE (OP) (OR (GET OP '*CPRECEDENCE*) 8)) 
(PUT 'OR '*CPRECEDENCE* 1) 
(PUT 'AND '*CPRECEDENCE* 2) 
(PUT 'EQUAL '*CPRECEDENCE* 3) 
(PUT 'NEQ '*CPRECEDENCE* 3) 
(PUT 'GREATERP '*CPRECEDENCE* 4) 
(PUT 'GEQ '*CPRECEDENCE* 4) 
(PUT 'LESSP '*CPRECEDENCE* 4) 
(PUT 'LEQ '*CPRECEDENCE* 4) 
(PUT 'PLUS '*CPRECEDENCE* 5) 
(PUT 'TIMES '*CPRECEDENCE* 6) 
(PUT 'QUOTIENT '*CPRECEDENCE* 6) 
(PUT 'NOT '*CPRECEDENCE* 7) 
(PUT 'MINUS '*CPRECEDENCE* 7) 
(PUT 'CSTMT 'NUMBER-OF-ARGS 1) 
(PUT 'CSTMT 'DEFINED-ON-LINE '338) 
(PUT 'CSTMT 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CSTMT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CSTMT (STMT)
    (COND ((NULL STMT) NIL) ((LISPLABELP STMT) (CLABEL STMT))
          ((EQ (CAR STMT) 'LITERAL) (CLITERAL STMT))
          ((LISPASSIGNP STMT) (CASSIGN STMT)) ((LISPCONDP STMT) (CIF STMT))
          ((LISPBREAKP STMT) (CBREAK STMT)) ((LISPGOP STMT) (CGOTO STMT))
          ((LISPRETURNP STMT) (CRETURN STMT)) ((LISPSTOPP STMT) (CEXIT STMT))
          ((LISPREPEATP STMT) (CREPEAT STMT)) ((LISPWHILEP STMT) (CWHILE STMT))
          ((LISPFORP STMT) (CFOR STMT)) ((LISPSTMTGPP STMT) (CSTMTGP STMT))
          ((LISPDEFP STMT) (CPROC STMT)) (T (CEXPSTMT STMT)))) 
(PUT 'CASSIGN 'NUMBER-OF-ARGS 1) 
(PUT 'CASSIGN 'DEFINED-ON-LINE '370) 
(PUT 'CASSIGN 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CASSIGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CASSIGN (STMT) (MKFCASSIGN (CADR STMT) (CADDR STMT))) 
(PUT 'CBREAK 'NUMBER-OF-ARGS 1) 
(PUT 'CBREAK 'DEFINED-ON-LINE '373) 
(PUT 'CBREAK 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CBREAK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CBREAK (STMT) (MKFCBREAK)) 
(PUT 'CEXIT 'NUMBER-OF-ARGS 1) 
(PUT 'CEXIT 'DEFINED-ON-LINE '376) 
(PUT 'CEXIT 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CEXIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CEXIT (STMT) (MKFCEXIT)) 
(PUT 'CEXPSTMT 'NUMBER-OF-ARGS 1) 
(PUT 'CEXPSTMT 'DEFINED-ON-LINE '379) 
(PUT 'CEXPSTMT 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CEXPSTMT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CEXPSTMT (EXP) (APPEND (CONS (MKCTAB) (CEXP EXP)) (LIST '|;| (MKCTERPRI)))) 
(PUT 'CFOR 'NUMBER-OF-ARGS 1) 
(PUT 'CFOR 'DEFINED-ON-LINE '382) 
(PUT 'CFOR 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CFOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CFOR (STMT)
    (PROG (R VAR LOEXP STEPEXP HIEXP STMTLST)
      (SETQ VAR (CADR STMT))
      (SETQ STMT (CDDR STMT))
      (SETQ LOEXP (CAAR STMT))
      (SETQ STEPEXP (CADAR STMT))
      (SETQ HIEXP (CADDAR STMT))
      (SETQ STMTLST (CDDR STMT))
      (SETQ R
              (MKFCFOR VAR LOEXP
               (LIST
                (COND
                 ((OR (AND (NUMBERP STEPEXP) (LESSP STEPEXP 0))
                      (EQCAR STEPEXP 'MINUS))
                  'GEQ)
                 (T 'LEQ))
                VAR HIEXP)
               VAR (LIST 'PLUS VAR STEPEXP)))
      (INDENTCLEVEL (PLUS 1))
      (SETQ R
              (APPEND R
                      (PROG (ST FORALL-RESULT FORALL-ENDPTR)
                        (SETQ ST STMTLST)
                       STARTOVER
                        (COND ((NULL ST) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (ST) (CSTMT ST)) (CAR ST)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ ST (CDR ST))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL ST) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (ST) (CSTMT ST)) (CAR ST)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ ST (CDR ST))
                        (GO LOOPLABEL))))
      (INDENTCLEVEL (MINUS 1))
      (RETURN R))) 
(PUT 'CGOTO 'NUMBER-OF-ARGS 1) 
(PUT 'CGOTO 'DEFINED-ON-LINE '403) 
(PUT 'CGOTO 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CGOTO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CGOTO (STMT) (MKFCGO (CADR STMT))) 
(PUT 'CIF 'NUMBER-OF-ARGS 1) 
(PUT 'CIF 'DEFINED-ON-LINE '406) 
(PUT 'CIF 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CIF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CIF (STMT)
    (PROG (R ST)
      (SETQ R (MKFCIF (CAADR STMT)))
      (INDENTCLEVEL (PLUS 1))
      (SETQ ST (SEQTOGP (CDADR STMT)))
      (COND
       ((AND (EQCAR ST 'COND) (EQUAL (LENGTH ST) 2))
        (SETQ ST (MKSTMTGP 0 (LIST ST)))))
      (SETQ R (APPEND R (CSTMT ST)))
      (INDENTCLEVEL (MINUS 1))
      (SETQ STMT (CDR STMT))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (AND (SETQ STMT (CDR STMT)) (NEQ (CAAR STMT) T))) (RETURN NIL)))
        (PROGN
         (SETQ R (APPEND R (MKFCELSEIF (CAAR STMT))))
         (INDENTCLEVEL (PLUS 1))
         (SETQ ST (SEQTOGP (CDAR STMT)))
         (COND
          ((AND (EQCAR ST 'COND) (EQUAL (LENGTH ST) 2))
           (SETQ ST (MKSTMTGP 0 (LIST ST)))))
         (SETQ R (APPEND R (CSTMT ST)))
         (INDENTCLEVEL (MINUS 1)))
        (GO WHILELABEL))
      (COND
       (STMT
        (PROGN
         (SETQ R (APPEND R (MKFCELSE)))
         (INDENTCLEVEL (PLUS 1))
         (SETQ ST (SEQTOGP (CDAR STMT)))
         (COND
          ((AND (EQCAR ST 'COND) (EQUAL (LENGTH ST) 2))
           (SETQ ST (MKSTMTGP 0 (LIST ST)))))
         (SETQ R (APPEND R (CSTMT ST)))
         (INDENTCLEVEL (MINUS 1)))))
      (RETURN R))) 
(PUT 'CLABEL 'NUMBER-OF-ARGS 1) 
(PUT 'CLABEL 'DEFINED-ON-LINE '440) 
(PUT 'CLABEL 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CLABEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLABEL (LABEL) (MKFCLABEL LABEL)) 
(PUT 'CLITERAL 'NUMBER-OF-ARGS 1) 
(PUT 'CLITERAL 'DEFINED-ON-LINE '443) 
(PUT 'CLITERAL 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CLITERAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLITERAL (STMT) (MKFCLITERAL (CDR STMT))) 
(PUT 'CREPEAT 'NUMBER-OF-ARGS 1) 
(PUT 'CREPEAT 'DEFINED-ON-LINE '446) 
(PUT 'CREPEAT 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CREPEAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CREPEAT (STMT)
    (PROG (R STMTLST LOGEXP)
      (SETQ STMT (REVERSE (CDR STMT)))
      (SETQ LOGEXP (CAR STMT))
      (SETQ STMTLST (REVERSE (CDR STMT)))
      (SETQ R (MKFCDO))
      (INDENTCLEVEL (PLUS 1))
      (SETQ R
              (APPEND R
                      (PROG (ST FORALL-RESULT FORALL-ENDPTR)
                        (SETQ ST STMTLST)
                       STARTOVER
                        (COND ((NULL ST) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (ST) (CSTMT ST)) (CAR ST)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ ST (CDR ST))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL ST) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (ST) (CSTMT ST)) (CAR ST)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ ST (CDR ST))
                        (GO LOOPLABEL))))
      (INDENTCLEVEL (MINUS 1))
      (RETURN (APPEND R (MKFCDOWHILE (LIST 'NOT LOGEXP)))))) 
(PUT 'CRETURN 'NUMBER-OF-ARGS 1) 
(PUT 'CRETURN 'DEFINED-ON-LINE '459) 
(PUT 'CRETURN 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CRETURN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CRETURN (STMT)
    (COND ((CDR STMT) (MKFCRETURN (CADR STMT))) (T (MKFCRETURN NIL)))) 
(PUT 'CSTMTGP 'NUMBER-OF-ARGS 1) 
(PUT 'CSTMTGP 'DEFINED-ON-LINE '465) 
(PUT 'CSTMTGP 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CSTMTGP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CSTMTGP (STMTGP)
    (PROG (R)
      (COND ((EQ (CAR STMTGP) 'PROGN) (SETQ STMTGP (CDR STMTGP)))
            (T (SETQ STMTGP (CDDR STMTGP))))
      (SETQ R (MKFCBEGINGP))
      (INDENTCLEVEL (PLUS 1))
      (SETQ R
              (APPEND R
                      (PROG (STMT FORALL-RESULT FORALL-ENDPTR)
                        (SETQ STMT STMTGP)
                       STARTOVER
                        (COND ((NULL STMT) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (STMT) (CSTMT STMT)) (CAR STMT)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ STMT (CDR STMT))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL STMT) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (STMT) (CSTMT STMT)) (CAR STMT)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ STMT (CDR STMT))
                        (GO LOOPLABEL))))
      (INDENTCLEVEL (MINUS 1))
      (RETURN (APPEND R (MKFCENDGP))))) 
(PUT 'CWHILE 'NUMBER-OF-ARGS 1) 
(PUT 'CWHILE 'DEFINED-ON-LINE '479) 
(PUT 'CWHILE 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CWHILE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CWHILE (STMT)
    (PROG (R LOGEXP STMTLST)
      (SETQ LOGEXP (CADR STMT))
      (SETQ STMTLST (CDDR STMT))
      (SETQ R (MKFCWHILE LOGEXP))
      (INDENTCLEVEL (PLUS 1))
      (SETQ R
              (APPEND R
                      (PROG (ST FORALL-RESULT FORALL-ENDPTR)
                        (SETQ ST STMTLST)
                       STARTOVER
                        (COND ((NULL ST) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (ST) (CSTMT ST)) (CAR ST)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ ST (CDR ST))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL ST) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (ST) (CSTMT ST)) (CAR ST)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ ST (CDR ST))
                        (GO LOOPLABEL))))
      (INDENTCLEVEL (MINUS 1))
      (RETURN R))) 
(DE CEXP_NAME (U)
    (COND ((ATOM U) (LIST U))
          (T (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR U))) (CAR U))))) 
(PUT 'CEXP_NAME 'NUMBER-OF-ARGS 1) 
(PUT 'CEXP_NAME 'DEFINED-ON-LINE '502) 
(PUT 'CEXP_NAME 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CEXP_NAME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'CEXP_NAME 'INLINE
      '(LAMBDA (U)
         (COND ((ATOM U) (LIST U))
               (T (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR U))) (CAR U)))))) 
(PUT 'MKFCASSIGN 'NUMBER-OF-ARGS 2) 
(PUT 'MKFCASSIGN 'DEFINED-ON-LINE '506) 
(PUT 'MKFCASSIGN 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCASSIGN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKFCASSIGN (LHS RHS)
    (PROG (ST)
      (COND
       ((AND (EQUAL (LENGTH RHS) 3) (MEMBER LHS RHS))
        (PROG (OP EXP1 EXP2)
          (SETQ OP (CAR RHS))
          (SETQ EXP1 (CADR RHS))
          (SETQ EXP2 (CADDR RHS))
          (COND
           ((EQUAL OP 'PLUS)
            (COND
             ((OR (ONEP EXP1) (ONEP EXP2))
              (SETQ ST
                      (CONS '++
                            (COND ((ATOM LHS) (LIST LHS))
                                  (T
                                   (RPLACA
                                    (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                    (CAR LHS)))))))
             ((OR (MEMBER EXP1 '(-1 (MINUS 1))) (MEMBER EXP2 '(-1 (MINUS 1))))
              (SETQ ST
                      (CONS '--
                            (COND ((ATOM LHS) (LIST LHS))
                                  (T
                                   (RPLACA
                                    (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                    (CAR LHS)))))))
             ((EQCAR EXP1 'MINUS)
              (SETQ ST
                      (APPEND
                       (COND ((ATOM LHS) (LIST LHS))
                             (T
                              (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                      (CAR LHS))))
                       (CONS '-= (CEXP (CADR EXP1))))))
             ((EQCAR EXP2 'MINUS)
              (SETQ ST
                      (APPEND
                       (COND ((ATOM LHS) (LIST LHS))
                             (T
                              (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                      (CAR LHS))))
                       (CONS '-= (CEXP (CADR EXP2))))))
             ((EQUAL EXP1 LHS)
              (SETQ ST
                      (APPEND
                       (COND ((ATOM LHS) (LIST LHS))
                             (T
                              (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                      (CAR LHS))))
                       (CONS '+= (CEXP EXP2)))))
             (T
              (SETQ ST
                      (APPEND
                       (COND ((ATOM LHS) (LIST LHS))
                             (T
                              (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                      (CAR LHS))))
                       (CONS '+= (CEXP EXP1)))))))
           ((AND (EQUAL OP 'DIFFERENCE) (ONEP EXP2))
            (SETQ ST
                    (CONS '--
                          (COND ((ATOM LHS) (LIST LHS))
                                (T
                                 (RPLACA
                                  (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                  (CAR LHS)))))))
           ((AND (EQUAL OP 'DIFFERENCE) (EQUAL EXP1 LHS))
            (SETQ ST
                    (APPEND
                     (COND ((ATOM LHS) (LIST LHS))
                           (T
                            (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                    (CAR LHS))))
                     (CONS '-= (CEXP EXP2)))))
           ((AND (EQUAL OP 'TIMES) (EQUAL EXP1 LHS))
            (SETQ ST
                    (APPEND
                     (COND ((ATOM LHS) (LIST LHS))
                           (T
                            (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                    (CAR LHS))))
                     (CONS '*= (CEXP EXP2)))))
           ((EQUAL OP 'TIMES)
            (SETQ ST
                    (APPEND
                     (COND ((ATOM LHS) (LIST LHS))
                           (T
                            (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                    (CAR LHS))))
                     (CONS '*= (CEXP EXP1)))))
           ((AND (EQUAL OP 'QUOTIENT) (EQUAL EXP1 LHS))
            (SETQ ST
                    (APPEND
                     (COND ((ATOM LHS) (LIST LHS))
                           (T
                            (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                    (CAR LHS))))
                     (CONS '/= (CEXP EXP2)))))
           (T
            (SETQ ST
                    (APPEND
                     (COND ((ATOM LHS) (LIST LHS))
                           (T
                            (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                    (CAR LHS))))
                     (CONS '= (CEXP RHS))))))))
       (T
        (SETQ ST
                (APPEND
                 (COND ((ATOM LHS) (LIST LHS))
                       (T
                        (RPLACA (CEXP (CONS 'DUMMYARRAYTOKEN (CDR LHS)))
                                (CAR LHS))))
                 (CONS '= (CEXP RHS))))))
      (RETURN (APPEND (CONS (MKCTAB) ST) (LIST '|;| (MKCTERPRI)))))) 
(PUT 'MKFCBEGINGP 'NUMBER-OF-ARGS 0) 
(PUT 'MKFCBEGINGP 'DEFINED-ON-LINE '547) 
(PUT 'MKFCBEGINGP 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCBEGINGP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKFCBEGINGP NIL (LIST (MKCTAB) '{ (MKCTERPRI))) 
(PUT 'MKFCBREAK 'NUMBER-OF-ARGS 0) 
(PUT 'MKFCBREAK 'DEFINED-ON-LINE '550) 
(PUT 'MKFCBREAK 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCBREAK 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKFCBREAK NIL (LIST (MKCTAB) 'BREAK '|;| (MKCTERPRI))) 
(PUT 'MKFCDEC 'NUMBER-OF-ARGS 2) 
(PUT 'MKFCDEC 'DEFINED-ON-LINE '553) 
(PUT 'MKFCDEC 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCDEC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKFCDEC (TYPE VARLIST)
    (PROGN
     (COND ((EQUAL TYPE 'SCALAR) (SETQ TYPE DEFTYPE*)))
     (COND ((EQUAL TYPE 'REAL) (SETQ TYPE 'FLOAT))
           ((EQUAL TYPE 'INTEGER) (SETQ TYPE 'INT)))
     (COND
      (*DOUBLE
       (COND ((EQUAL TYPE 'FLOAT) (SETQ TYPE 'DOUBLE))
             ((EQUAL TYPE 'INT) (SETQ TYPE 'LONG)))))
     (SETQ VARLIST
             (PROG (V FORALL-RESULT FORALL-ENDPTR)
               (SETQ V VARLIST)
               (COND ((NULL V) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (V)
                                   (COND ((ATOM V) V)
                                         (T
                                          (CONS (CAR V)
                                                (PROG (DIM FORALL-RESULT
                                                       FORALL-ENDPTR)
                                                  (SETQ DIM (CDR V))
                                                  (COND
                                                   ((NULL DIM) (RETURN NIL)))
                                                  (SETQ FORALL-RESULT
                                                          (SETQ FORALL-ENDPTR
                                                                  (CONS
                                                                   ((LAMBDA
                                                                        (DIM)
                                                                      (COND
                                                                       ((EQ DIM
                                                                            'TIMES)
                                                                        BLANK)
                                                                       ((NUMBERP
                                                                         DIM)
                                                                        (ADD1
                                                                         DIM))
                                                                       ((AND
                                                                         (EQCAR
                                                                          DIM
                                                                          'RANGE)
                                                                         (EQUAL
                                                                          (CADR
                                                                           DIM)
                                                                          0))
                                                                        (ADD1
                                                                         (CADDR
                                                                          DIM)))
                                                                       (T
                                                                        (GENTRANERR
                                                                         'E DIM
                                                                         "Not C dimension"
                                                                         NIL))))
                                                                    (CAR DIM))
                                                                   NIL)))
                                                 LOOPLABEL
                                                  (SETQ DIM (CDR DIM))
                                                  (COND
                                                   ((NULL DIM)
                                                    (RETURN FORALL-RESULT)))
                                                  (RPLACD FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (DIM)
                                                              (COND
                                                               ((EQ DIM 'TIMES)
                                                                BLANK)
                                                               ((NUMBERP DIM)
                                                                (ADD1 DIM))
                                                               ((AND
                                                                 (EQCAR DIM
                                                                        'RANGE)
                                                                 (EQUAL
                                                                  (CADR DIM)
                                                                  0))
                                                                (ADD1
                                                                 (CADDR DIM)))
                                                               (T
                                                                (GENTRANERR 'E
                                                                 DIM
                                                                 "Not C dimension"
                                                                 NIL))))
                                                            (CAR DIM))
                                                           NIL))
                                                  (SETQ FORALL-ENDPTR
                                                          (CDR FORALL-ENDPTR))
                                                  (GO LOOPLABEL))))))
                                 (CAR V))
                                NIL)))
              LOOPLABEL
               (SETQ V (CDR V))
               (COND ((NULL V) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (V)
                           (COND ((ATOM V) V)
                                 (T
                                  (CONS (CAR V)
                                        (PROG (DIM FORALL-RESULT FORALL-ENDPTR)
                                          (SETQ DIM (CDR V))
                                          (COND ((NULL DIM) (RETURN NIL)))
                                          (SETQ FORALL-RESULT
                                                  (SETQ FORALL-ENDPTR
                                                          (CONS
                                                           ((LAMBDA (DIM)
                                                              (COND
                                                               ((EQ DIM 'TIMES)
                                                                BLANK)
                                                               ((NUMBERP DIM)
                                                                (ADD1 DIM))
                                                               ((AND
                                                                 (EQCAR DIM
                                                                        'RANGE)
                                                                 (EQUAL
                                                                  (CADR DIM)
                                                                  0))
                                                                (ADD1
                                                                 (CADDR DIM)))
                                                               (T
                                                                (GENTRANERR 'E
                                                                 DIM
                                                                 "Not C dimension"
                                                                 NIL))))
                                                            (CAR DIM))
                                                           NIL)))
                                         LOOPLABEL
                                          (SETQ DIM (CDR DIM))
                                          (COND
                                           ((NULL DIM) (RETURN FORALL-RESULT)))
                                          (RPLACD FORALL-ENDPTR
                                                  (CONS
                                                   ((LAMBDA (DIM)
                                                      (COND
                                                       ((EQ DIM 'TIMES) BLANK)
                                                       ((NUMBERP DIM)
                                                        (ADD1 DIM))
                                                       ((AND (EQCAR DIM 'RANGE)
                                                             (EQUAL (CADR DIM)
                                                                    0))
                                                        (ADD1 (CADDR DIM)))
                                                       (T
                                                        (GENTRANERR 'E DIM
                                                         "Not C dimension"
                                                         NIL))))
                                                    (CAR DIM))
                                                   NIL))
                                          (SETQ FORALL-ENDPTR
                                                  (CDR FORALL-ENDPTR))
                                          (GO LOOPLABEL))))))
                         (CAR V))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
     (APPEND
      (CONS (MKCTAB)
            (CONS TYPE
                  (CONS BLANK
                        (PROG (V FORALL-RESULT FORALL-ENDPTR)
                          (SETQ V (INSERTCOMMAS VARLIST))
                         STARTOVER
                          (COND ((NULL V) (RETURN NIL)))
                          (SETQ FORALL-RESULT
                                  ((LAMBDA (V)
                                     (COND ((ATOM V) (LIST V))
                                           (T
                                            (RPLACA
                                             (CEXP
                                              (CONS 'DUMMYARRAYTOKEN (CDR V)))
                                             (CAR V)))))
                                   (CAR V)))
                          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                          (SETQ V (CDR V))
                          (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                         LOOPLABEL
                          (COND ((NULL V) (RETURN FORALL-RESULT)))
                          (RPLACD FORALL-ENDPTR
                                  ((LAMBDA (V)
                                     (COND ((ATOM V) (LIST V))
                                           (T
                                            (RPLACA
                                             (CEXP
                                              (CONS 'DUMMYARRAYTOKEN (CDR V)))
                                             (CAR V)))))
                                   (CAR V)))
                          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                          (SETQ V (CDR V))
                          (GO LOOPLABEL)))))
      (LIST '|;| (MKCTERPRI))))) 
(PUT 'MKFCDO 'NUMBER-OF-ARGS 0) 
(PUT 'MKFCDO 'DEFINED-ON-LINE '586) 
(PUT 'MKFCDO 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCDO 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKFCDO NIL (LIST (MKCTAB) 'DO (MKCTERPRI))) 
(PUT 'MKFCDOWHILE 'NUMBER-OF-ARGS 1) 
(PUT 'MKFCDOWHILE 'DEFINED-ON-LINE '589) 
(PUT 'MKFCDOWHILE 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCDOWHILE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKFCDOWHILE (EXP)
    (APPEND (APPEND (LIST (MKCTAB) 'WHILE BLANK '|(|) (CEXP EXP))
            (LIST '|)| '|;| (MKCTERPRI)))) 
(PUT 'MKFCELSE 'NUMBER-OF-ARGS 0) 
(PUT 'MKFCELSE 'DEFINED-ON-LINE '593) 
(PUT 'MKFCELSE 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCELSE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKFCELSE NIL (LIST (MKCTAB) 'ELSE (MKCTERPRI))) 
(PUT 'MKFCELSEIF 'NUMBER-OF-ARGS 1) 
(PUT 'MKFCELSEIF 'DEFINED-ON-LINE '596) 
(PUT 'MKFCELSEIF 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCELSEIF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKFCELSEIF (EXP)
    (APPEND (APPEND (LIST (MKCTAB) 'ELSE BLANK 'IF BLANK '|(|) (CEXP EXP))
            (LIST '|)| (MKCTERPRI)))) 
(PUT 'MKFCENDGP 'NUMBER-OF-ARGS 0) 
(PUT 'MKFCENDGP 'DEFINED-ON-LINE '601) 
(PUT 'MKFCENDGP 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCENDGP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKFCENDGP NIL (LIST (MKCTAB) '} (MKCTERPRI))) 
(PUT 'MKFCEXIT 'NUMBER-OF-ARGS 0) 
(PUT 'MKFCEXIT 'DEFINED-ON-LINE '604) 
(PUT 'MKFCEXIT 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCEXIT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKFCEXIT NIL (LIST (MKCTAB) 'EXIT '|(| 0 '|)| '|;| (MKCTERPRI))) 
(PUT 'MKFCFOR 'NUMBER-OF-ARGS 5) 
(PUT 'MKFCFOR 'DEFINED-ON-LINE '607) 
(PUT 'MKFCFOR 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCFOR 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKFCFOR (VAR1 LO COND VAR2 NEXTEXP)
    (PROGN
     (COND (VAR1 (SETQ VAR1 (APPEND (CEXP VAR1) (CONS '= (CEXP LO))))))
     (COND (COND (SETQ COND (CEXP COND))))
     (COND
      (VAR2
       (PROGN
        (SETQ VAR2 (CDR (MKFCASSIGN VAR2 NEXTEXP)))
        (SETQ VAR2 (REVERSE (CDDR (REVERSE VAR2)))))))
     (APPEND
      (APPEND (APPEND (LIST (MKCTAB) '|FOR | BLANK '|(|) VAR1)
              (CONS '|;| COND))
      (APPEND (CONS '|;| VAR2) (LIST '|)| (MKCTERPRI)))))) 
(PUT 'MKFCGO 'NUMBER-OF-ARGS 1) 
(PUT 'MKFCGO 'DEFINED-ON-LINE '623) 
(PUT 'MKFCGO 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCGO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKFCGO (LABEL) (LIST (MKCTAB) 'GOTO BLANK LABEL '|;| (MKCTERPRI))) 
(PUT 'MKFCIF 'NUMBER-OF-ARGS 1) 
(PUT 'MKFCIF 'DEFINED-ON-LINE '626) 
(PUT 'MKFCIF 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCIF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKFCIF (EXP)
    (APPEND (APPEND (LIST (MKCTAB) 'IF BLANK '|(|) (CEXP EXP))
            (LIST '|)| (MKCTERPRI)))) 
(PUT 'MKFCLABEL 'NUMBER-OF-ARGS 1) 
(PUT 'MKFCLABEL 'DEFINED-ON-LINE '630) 
(PUT 'MKFCLABEL 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCLABEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKFCLABEL (LABEL) (LIST LABEL '|:| (MKCTERPRI))) 
(PUT 'MKFCLITERAL 'NUMBER-OF-ARGS 1) 
(PUT 'MKFCLITERAL 'DEFINED-ON-LINE '633) 
(PUT 'MKFCLITERAL 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCLITERAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKFCLITERAL (ARGS)
    (PROG (A FORALL-RESULT FORALL-ENDPTR)
      (SETQ A ARGS)
     STARTOVER
      (COND ((NULL A) (RETURN NIL)))
      (SETQ FORALL-RESULT
              ((LAMBDA (A)
                 (COND ((EQ A 'TAB*) (LIST (MKCTAB)))
                       ((EQ A 'CR*) (LIST (MKCTERPRI))) ((PAIRP A) (CEXP A))
                       (T (LIST (STRIPQUOTES A)))))
               (CAR A)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
      (SETQ A (CDR A))
      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
     LOOPLABEL
      (COND ((NULL A) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              ((LAMBDA (A)
                 (COND ((EQ A 'TAB*) (LIST (MKCTAB)))
                       ((EQ A 'CR*) (LIST (MKCTERPRI))) ((PAIRP A) (CEXP A))
                       (T (LIST (STRIPQUOTES A)))))
               (CAR A)))
      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
      (SETQ A (CDR A))
      (GO LOOPLABEL))) 
(PUT 'MKFCPROCDEC 'NUMBER-OF-ARGS 3) 
(PUT 'MKFCPROCDEC 'DEFINED-ON-LINE '644) 
(PUT 'MKFCPROCDEC 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCPROCDEC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKFCPROCDEC (TYPE NAME PARAMS)
    (PROGN
     (SETQ PARAMS
             (APPEND
              (CONS '|(|
                    (PROG (P FORALL-RESULT FORALL-ENDPTR)
                      (SETQ P (INSERTCOMMAS PARAMS))
                     STARTOVER
                      (COND ((NULL P) (RETURN NIL)))
                      (SETQ FORALL-RESULT ((LAMBDA (P) (CEXP P)) (CAR P)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ P (CDR P))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL P) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR ((LAMBDA (P) (CEXP P)) (CAR P)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ P (CDR P))
                      (GO LOOPLABEL)))
              (LIST '|)|)))
     (COND
      (TYPE
       (APPEND (CONS (MKCTAB) (CONS TYPE (CONS BLANK (CEXP NAME))))
               (APPEND PARAMS (LIST (MKCTERPRI)))))
      (T
       (APPEND (CONS (MKCTAB) (CEXP NAME))
               (APPEND PARAMS (LIST (MKCTERPRI)))))))) 
(PUT 'MKFCRETURN 'NUMBER-OF-ARGS 1) 
(PUT 'MKFCRETURN 'DEFINED-ON-LINE '656) 
(PUT 'MKFCRETURN 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCRETURN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKFCRETURN (EXP)
    (COND
     (EXP
      (APPEND (APPEND (LIST (MKCTAB) 'RETURN '|(|) (CEXP EXP))
              (LIST '|)| '|;| (MKCTERPRI))))
     (T (LIST (MKCTAB) 'RETURN '|;| (MKCTERPRI))))) 
(PUT 'MKFCWHILE 'NUMBER-OF-ARGS 1) 
(PUT 'MKFCWHILE 'DEFINED-ON-LINE '663) 
(PUT 'MKFCWHILE 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKFCWHILE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKFCWHILE (EXP)
    (APPEND (APPEND (LIST (MKCTAB) 'WHILE BLANK '|(|) (CEXP EXP))
            (LIST '|)| (MKCTERPRI)))) 
(PUT 'MKCTAB 'NUMBER-OF-ARGS 0) 
(PUT 'MKCTAB 'DEFINED-ON-LINE '671) 
(PUT 'MKCTAB 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKCTAB 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKCTAB NIL (LIST 'CTAB CCURRIND*)) 
(PUT 'INDENTCLEVEL 'NUMBER-OF-ARGS 1) 
(PUT 'INDENTCLEVEL 'DEFINED-ON-LINE '675) 
(PUT 'INDENTCLEVEL 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'INDENTCLEVEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDENTCLEVEL (N)
    (SETQ CCURRIND*
            (PROGN
             (SETQ ALGLIST* (CONS NIL NIL))
             (PLUS CCURRIND* (TIMES N TABLEN*))))) 
(PUT 'MKCTERPRI 'NUMBER-OF-ARGS 0) 
(PUT 'MKCTERPRI 'DEFINED-ON-LINE '679) 
(PUT 'MKCTERPRI 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'MKCTERPRI 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKCTERPRI NIL (LIST 'CTERPRI)) 
(PUT 'INSERTBRACKETS 'NUMBER-OF-ARGS 1) 
(PUT 'INSERTBRACKETS 'DEFINED-ON-LINE '688) 
(PUT 'INSERTBRACKETS 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'INSERTBRACKETS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INSERTBRACKETS (EXP) (CONS '[ (APPEND EXP (LIST '])))) 
(PUT 'FORMATC 'NUMBER-OF-ARGS 1) 
(PUT 'FORMATC 'DEFINED-ON-LINE '695) 
(PUT 'FORMATC 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'FORMATC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FORMATC (LST)
    (PROG (LINELEN)
      (SETQ LINELEN (LINELENGTH 300))
      (SETQ *POSN* 0)
      (PROG (ELT)
        (SETQ ELT LST)
       LAB
        (COND ((NULL ELT) (RETURN NIL)))
        ((LAMBDA (ELT)
           (COND ((PAIRP ELT) (LISPEVAL ELT))
                 (T
                  (PROGN
                   (COND
                    ((GREATERP (PLUS *POSN* (LENGTH (EXPLODE2 ELT))) CLINELEN*)
                     (CCONTLINE)))
                   (PPRIN2 ELT)))))
         (CAR ELT))
        (SETQ ELT (CDR ELT))
        (GO LAB))
      (LINELENGTH LINELEN))) 
(PUT 'CCONTLINE 'NUMBER-OF-ARGS 0) 
(PUT 'CCONTLINE 'DEFINED-ON-LINE '711) 
(PUT 'CCONTLINE 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CCONTLINE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CCONTLINE NIL (PROGN (CTERPRI) (CTAB *CCURRIND*) (PPRIN2 " "))) 
(PUT 'CTERPRI 'NUMBER-OF-ARGS 0) 
(PUT 'CTERPRI 'DEFINED-ON-LINE '718) 
(PUT 'CTERPRI 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CTERPRI 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CTERPRI NIL (PTERPRI)) 
(PUT 'CTAB 'NUMBER-OF-ARGS 1) 
(PUT 'CTAB 'DEFINED-ON-LINE '721) 
(PUT 'CTAB 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'CTAB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CTAB (N)
    (PROGN
     (SETQ *CCURRIND* (MIN0 N (DIFFERENCE CLINELEN* MINCLINELEN*)))
     (COND
      ((GREATERP (SETQ N (DIFFERENCE *CCURRIND* *POSN*)) 0)
       (PPRIN2 (NSPACES N)))))) 
(PUT 'PROCCTEM 'NUMBER-OF-ARGS 0) 
(PUT 'PROCCTEM 'DEFINED-ON-LINE '730) 
(PUT 'PROCCTEM 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'PROCCTEM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PROCCTEM NIL
    (PROG (C LINELEN)
      (SETQ LINELEN (LINELENGTH 150))
      (SETQ C (READCH))
      (COND ((EQ C '|#|) (SETQ C (|PROCC#LINE| C))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ C $EOF$)) (RETURN NIL)))
        (COND ((EQ C $EOL$) (SETQ C (|PROCC#LINE| C)))
              ((EQ C '/) (SETQ C (PROCCCOMM)))
              ((EQ C '|;|) (SETQ C (PROCACTIVE))) (T (SETQ C (PROCCHEADER C))))
        (GO WHILELABEL))
      (LINELENGTH LINELEN))) 
(PUT '|PROCC#LINE| 'NUMBER-OF-ARGS 1) 
(PUT '|PROCC#LINE| 'DEFINED-ON-LINE '748) 
(PUT '|PROCC#LINE| 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT '|PROCC#LINE| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PROCC#LINE| (C)
    (PROG ()
      (COND ((EQ C $EOL$) (PROGN (PTERPRI) (SETQ C (READCH)))))
      (COND
       ((EQ C '|#|)
        (PROG ()
         REPEATLABEL
          (PROGN (PPRIN2 C) (SETQ C (READCH)))
          (COND ((NOT (EQ C $EOL$)) (GO REPEATLABEL))))))
      (RETURN C))) 
(PUT 'PROCCCOMM 'NUMBER-OF-ARGS 0) 
(PUT 'PROCCCOMM 'DEFINED-ON-LINE '760) 
(PUT 'PROCCCOMM 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'PROCCCOMM 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PROCCCOMM NIL
    (PROG (C)
      (PPRIN2 '/)
      (SETQ C (READCH))
      (COND
       ((EQ C '*)
        (PROGN
         (PPRIN2 C)
         (SETQ C (READCH))
         (PROG ()
          REPEATLABEL
           (PROGN
            (PROG ()
             WHILELABEL
              (COND ((NOT (NEQ C '*)) (RETURN NIL)))
              (PROGN
               (COND ((EQ C $EOL$) (PTERPRI)) (T (PPRIN2 C)))
               (SETQ C (READCH)))
              (GO WHILELABEL))
            (PPRIN2 C)
            (SETQ C (READCH)))
           (COND ((NOT (EQ C '/)) (GO REPEATLABEL))))
         (PPRIN2 C)
         (SETQ C (READCH)))))
      (RETURN C))) 
(PUT 'PROCCHEADER 'NUMBER-OF-ARGS 1) 
(PUT 'PROCCHEADER 'DEFINED-ON-LINE '789) 
(PUT 'PROCCHEADER 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'PROCCHEADER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PROCCHEADER (C)
    (PROG (NAME I)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (SEPRP C) (NEQ C $EOL$))) (RETURN NIL)))
        (PROGN (PPRIN2 C) (SETQ C (READCH)))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (NOT (OR (SEPRP C) (MEMQ C (LIST '/ '|;| '|(|)))))
          (RETURN NIL)))
        (PROGN (SETQ NAME (ACONC NAME C)) (PPRIN2 C) (SETQ C (READCH)))
        (GO WHILELABEL))
      (COND ((MEMQ C (LIST $EOL$ '/ '|;|)) (RETURN C)))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (SEPRP C) (NEQ C $EOL$))) (RETURN NIL)))
        (PROGN (PPRIN2 C) (SETQ C (READCH)))
        (GO WHILELABEL))
      (COND ((NEQ C '|(|) (RETURN C)))
      (SETQ NAME (INTERN (COMPRESS NAME)))
      (COND ((NOT *GENDECS) (SYMTABPUT NAME NIL NIL)))
      (PUT '$0 '*CNAME* NAME)
      (PPRIN2 C)
      (SETQ I 1)
      (SETQ C (READCH))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ C '|)|)) (RETURN NIL)))
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT (OR (SEPRP C) (EQ C '|,|))) (RETURN NIL)))
           (PROGN
            (COND ((EQ C $EOL$) (PTERPRI)) (T (PPRIN2 C)))
            (SETQ C (READCH)))
           (GO WHILELABEL))
         (SETQ NAME (LIST C))
         (PPRIN2 C)
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (NOT (OR (SEPRP (SETQ C (READCH))) (MEMQ C (LIST '|,| '|)|)))))
             (RETURN NIL)))
           (PROGN (SETQ NAME (ACONC NAME C)) (PPRIN2 C))
           (GO WHILELABEL))
         (PUT (INTERN (COMPRESS (APPEND (EXPLODE2 '$) (EXPLODE2 I)))) '*CNAME*
              (INTERN (COMPRESS NAME)))
         (SETQ I (ADD1 I))
         (SETQ C (FLUSHSPACES C)))
        (GO WHILELABEL))
      (SETQ |$#| (PROGN (SETQ ALGLIST* (CONS NIL NIL)) (SUB1 I)))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (GET
            (SETQ NAME (INTERN (COMPRESS (APPEND (EXPLODE2 '$) (EXPLODE2 I)))))
            '*CNAME*))
          (RETURN NIL)))
        (REMPROP NAME '*CNAME*)
        (GO WHILELABEL))
      (RETURN (PROCCFUNCTION C)))) 
(PUT 'PROCCFUNCTION 'NUMBER-OF-ARGS 1) 
(PUT 'PROCCFUNCTION 'DEFINED-ON-LINE '833) 
(PUT 'PROCCFUNCTION 'DEFINED-IN-FILE 'GENTRAN/LSPC.RED) 
(PUT 'PROCCFUNCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PROCCFUNCTION (C)
    (PROG ({}COUNT)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ C '{)) (RETURN NIL)))
        (COND ((EQ C '/) (SETQ C (PROCCCOMM)))
              ((EQ C '|;|) (SETQ C (PROCACTIVE)))
              ((EQ C $EOL$) (PROGN (PTERPRI) (SETQ C (READCH))))
              (T (PROGN (PPRIN2 C) (SETQ C (READCH)))))
        (GO WHILELABEL))
      (PPRIN2 C)
      (SETQ {}COUNT 1)
      (SETQ C (READCH))
      (PROG ()
       WHILELABEL
        (COND ((NOT (GREATERP {}COUNT 0)) (RETURN NIL)))
        (COND
         ((EQ C '{)
          (PROGN (SETQ {}COUNT (ADD1 {}COUNT)) (PPRIN2 C) (SETQ C (READCH))))
         ((EQ C '})
          (PROGN (SETQ {}COUNT (SUB1 {}COUNT)) (PPRIN2 C) (SETQ C (READCH))))
         ((EQ C '/) (SETQ C (PROCCCOMM))) ((EQ C '|;|) (SETQ C (PROCACTIVE)))
         ((EQ C $EOL$) (PROGN (PTERPRI) (SETQ C (READCH))))
         (T (PROGN (PPRIN2 C) (SETQ C (READCH)))))
        (GO WHILELABEL))
      (RETURN C))) 
(ENDMODULE) 