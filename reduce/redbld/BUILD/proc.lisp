(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PROC)) 
(FLUID
 '(*ARGNOCHK *NOINLINES *LOGINLINES *REDEFLG* FNAME* FTYPE* *STRICT_ARGCOUNT
   *COMP IFL* CURLINE*)) 
(SETQ *LOGINLINES T) 
(GLOBAL '(*LOSE *MICRO-VERSION CURSYM* CURESCAPED* ERFG* FTYPES*)) 
(FLUID '(*DEFN NEW_INLINE_DEFINITIONS)) 
(SETQ NEW_INLINE_DEFINITIONS NIL) 
(SETQ *LOSE T) 
(SETQ FTYPES* '(EXPR FEXPR MACRO)) 
(PUT 'EMB 'PROCFN 'PORTABLE-EMBFN) 
(PUT 'PORTABLE-EMBFN 'NUMBER-OF-ARGS 3) 
(PUT 'PORTABLE-EMBFN 'DEFINED-ON-LINE '66) 
(PUT 'PORTABLE-EMBFN 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'PORTABLE-EMBFN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PORTABLE-EMBFN (NAME ARGS BODY)
    (PROG (G)
      (COND
       ((NOT (EQCAR (GETD NAME) 'EXPR))
        (REDERR
         "Trying to embed around undefined function, or a fexpr or macro")))
      (SETQ G (GENSYM))
      (RETURN
       (LIST 'PROGN (LIST 'COPYD (MKQUOTE G) (MKQUOTE NAME))
             (LIST 'DE NAME ARGS (SSUBST G NAME BODY)))))) 
(PUT 'MKPROGN 'NUMBER-OF-ARGS 2) 
(PUT 'MKPROGN 'DEFINED-ON-LINE '85) 
(PUT 'MKPROGN 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'MKPROGN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKPROGN (U V)
    (COND ((EQCAR V 'PROGN) (CONS 'PROGN (CONS U (CDR V))))
          (T (LIST 'PROGN U V)))) 
(PUT 'PROC-ADD-INFO 'NUMBER-OF-ARGS 3) 
(PUT 'PROC-ADD-INFO 'DEFINED-ON-LINE '88) 
(PUT 'PROC-ADD-INFO 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'PROC-ADD-INFO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PROC-ADD-INFO (NAME INFO BODY)
    (COND ((NULL INFO) BODY)
          (T
           (PROC-ADD-INFO NAME (CDR INFO)
                          (MKPROGN
                           (LIST 'PUT (MKQUOTE NAME) (MKQUOTE (CAAR INFO))
                                 (MKQUOTE (CDAR INFO)))
                           BODY))))) 
(PUT 'FORMPROC 'NUMBER-OF-ARGS 3) 
(PUT 'FORMPROC 'DEFINED-ON-LINE '94) 
(PUT 'FORMPROC 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'FORMPROC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMPROC (U VARS MODE)
    (PROG (OBODY BODY FNAME* NAME TYPE VARLIS X Y FL N INFO)
      (SETQ U (CDR U))
      (SETQ NAME (SETQ FNAME* (CAR U)))
      (COND ((CADR U) (SETQ MODE (CADR U))))
      (SETQ U (CDDR U))
      (SETQ TYPE (SETQ FTYPE* (CAR U)))
      (COND
       ((AND (FLAGP NAME 'LOSE) (OR *LOSE (NULL *DEFN)))
        (RETURN (PROGN (LPRIM (LIST NAME "not defined (LOSE flag)")) ''NIL)))
       ((AND (NEQ MODE 'SYMBOLIC) (EQ (GETTYPE NAME) 'OPERATOR))
        (RETURN
         (PROGN (LPRIM (LIST NAME "already defined as operator")) ''NIL)))
       ((AND *REDEFLG* (GETD NAME)) (LPRIM (LIST NAME "redefined"))))
      (SETQ VARLIS (CADR U))
      (PROG ()
       WHILELABEL
        (COND ((NOT VARLIS) (RETURN NIL)))
        (PROGN
         (COND
          ((OR (NULL (CAR VARLIS)) (EQUAL (CAR VARLIS) 'T))
           (RSVERR (CAR VARLIS))))
         (SETQ VARLIS (CDR VARLIS)))
        (GO WHILELABEL))
      (SETQ VARLIS (CADR U))
      (SETQ BODY (CADDR U))
      (COND ((PAIRP (CDDDR U)) (SETQ INFO (CADDDR U))))
      (SETQ X (COND ((EQCAR BODY 'RBLOCK) (CADR BODY)) (T NIL)))
      (SETQ Y (PAIRXVARS VARLIS X VARS MODE))
      (COND (X (SETQ BODY (CONS (CAR BODY) (RPLACA* (CDR BODY) (CDR Y))))))
      (SETQ BODY
              (COND ((FLAGP NAME 'FORMC) (FORMC BODY (CAR Y) MODE))
                    (T (FORM1 BODY (CAR Y) MODE))))
      (COND ((AND *NOINLINES (EQUAL TYPE 'INLINE)) (SETQ TYPE 'EXPR)))
      (SETQ OBODY BODY)
      (COND
       ((EQUAL TYPE 'INLINE)
        (PROG (DD)
          (SETQ DD (LIST 'LAMBDA VARLIS BODY))
          (COND
           ((NOT (EQUAL DD (GET NAME 'INLINE)))
            (PROGN
             (COND ((NOT (ZEROP (POSN))) (TERPRI)))
             (PRIN2 "+++ Record new inline definition:")
             (TERPRI)
             (PRINT (LIST 'DE NAME VARLIS BODY))
             (SETQ NEW_INLINE_DEFINITIONS
                     (CONS (CONS NAME DD) NEW_INLINE_DEFINITIONS))))))))
      (COND
       ((OR (AND (NOT (EQUAL TYPE 'INLINE)) (GET NAME 'INLINE))
            (AND (NOT (EQUAL TYPE 'SMACRO)) (GET NAME 'SMACRO)))
        (LPRIM (LIST "SMACRO/INLINE" NAME "redefined"))))
      (SYMBVARLST VARLIS BODY MODE)
      (COND ((EQUAL TYPE 'EXPR) (SETQ BODY (LIST 'DE NAME VARLIS BODY)))
            ((EQUAL TYPE 'FEXPR) (SETQ BODY (LIST 'DF NAME VARLIS BODY)))
            ((EQUAL TYPE 'MACRO) (SETQ BODY (LIST 'DM NAME VARLIS BODY)))
            ((SETQ X (GET TYPE 'PROCFN)) (RETURN (APPLY3 X NAME VARLIS BODY)))
            (T
             (PROGN
              (SETQ BODY
                      (LIST 'PUTC (MKQUOTE NAME) (MKQUOTE TYPE)
                            (MKQUOTE (LIST 'LAMBDA VARLIS BODY))))
              (COND (*DEFN (LISPEVAL BODY))))))
      (SETQ BODY (PROC-ADD-INFO NAME INFO BODY))
      (COND
       ((NOT (EQUAL MODE 'SYMBOLIC))
        (SETQ BODY
                (MKPROGN (LIST 'FLAG (MKQUOTE (LIST NAME)) (MKQUOTE 'OPFN))
                         BODY))))
      (COND
       ((AND *ARGNOCHK (MEMQ TYPE '(EXPR INLINE SMACRO)))
        (PROGN
         (COND
          ((AND (SETQ N (GET NAME 'NUMBER-OF-ARGS))
                (NOT (FLAGP NAME 'VARIADIC)) (NEQ N (LENGTH VARLIS)))
           (PROGN
            (COND
             (*STRICT_ARGCOUNT
              (LPRIE
               (LIST "Definition of" NAME
                     "different count from args previously called with")))
             (T
              (LPRIM
               (LIST NAME "defined with" (LENGTH VARLIS)
                     "but previously called with" N "arguments")))))))
         (SETQ BODY
                 (MKPROGN
                  (LIST 'PUT (MKQUOTE NAME) (MKQUOTE 'NUMBER-OF-ARGS)
                        (LENGTH VARLIS))
                  BODY)))))
      (COND
       ((AND *DEFN (MEMQ TYPE '(FEXPR MACRO INLINE SMACRO))) (LISPEVAL BODY)))
      (COND
       ((EQUAL TYPE 'INLINE)
        (SETQ BODY (MKPROGN (LIST 'DE NAME VARLIS OBODY) BODY))))
      (RETURN
       (COND ((AND *MICRO-VERSION (MEMQ TYPE '(FEXPR MACRO SMACRO))) NIL)
             (T BODY))))) 
(PUT 'PROCEDURE 'FORMFN 'FORMPROC) 
(PUT 'FORMDE 'NUMBER-OF-ARGS 3) 
(PUT 'FORMDE 'DEFINED-ON-LINE '217) 
(PUT 'FORMDE 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'FORMDE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMDE (U VARS MODE)
    (COND
     ((EQUAL MODE 'SYMBOLIC)
      (FORMPROC
       (LIST 'PROCEDURE (CADR U) 'SYMBOLIC 'EXPR (CADDR U)
             (COND ((NULL (CDDDDR U)) (CADDDR U)) (T (CONS 'PROGN (CDDDR U)))))
       VARS MODE))
     (T (CONS 'LIST (CONS (ALGID (CAR U) VARS) (FORMLIS (CDR U) VARS MODE)))))) 
(PUT 'DE 'FORMFN 'FORMDE) 
(PUT 'PAIRXVARS 'NUMBER-OF-ARGS 4) 
(PUT 'PAIRXVARS 'DEFINED-ON-LINE '228) 
(PUT 'PAIRXVARS 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'PAIRXVARS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PAIRXVARS (U V VARS MODE)
    (PROG (X Y)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (COND
          ((SETQ Y (ATSOC (CAR U) V))
           (PROGN
            (SETQ V (DELETE Y V))
            (COND
             ((NOT (EQUAL (CDR Y) 'SCALAR))
              (SETQ X (CONS (CONS (CAR U) (CDR Y)) X)))
             (T (SETQ X (CONS (CONS (CAR U) MODE) X))))))
          ((OR (NULL (IDP (CAR U))) (GET (CAR U) 'INFIX) (GET (CAR U) 'STAT))
           (SYMERR (LIST "Invalid parameter:" (CAR U)) NIL))
          (T (SETQ X (CONS (CONS (CAR U) MODE) X))))
         (SETQ U (CDR U)))
        (GO WHILELABEL))
      (RETURN (CONS (APPEND (REVERSIP* X) VARS) V)))) 
(PUT 'STARTS-WITH 'NUMBER-OF-ARGS 2) 
(PUT 'STARTS-WITH 'DEFINED-ON-LINE '245) 
(PUT 'STARTS-WITH 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'STARTS-WITH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE STARTS-WITH (A B)
    (COND ((NULL B) T) ((NULL A) NIL)
          ((OR (EQCAR A (CAR B)) (AND (EQCAR A '|\\|) (EQCAR B '/)))
           (STARTS-WITH (CDR A) (CDR B)))
          (T NIL))) 
(PUT 'SIMPLIFY-FILENAME 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPLIFY-FILENAME 'DEFINED-ON-LINE '252) 
(PUT 'SIMPLIFY-FILENAME 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'SIMPLIFY-FILENAME 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPLIFY-FILENAME (S)
    (PROG (A B)
      (SETQ A (EXPLODE2 S))
      (SETQ B (EXPLODE2 "/packages/"))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND A (NOT (STARTS-WITH A B)))) (RETURN NIL)))
        (SETQ A (CDR A))
        (GO WHILELABEL))
      (COND ((NULL A) (RETURN S)))
      (SETQ A (CDDDDR (CDDDDR (CDDR A))))
      (RETURN (LIST2STRING A)))) 
(PUT 'MKHASH 'NUMBER-OF-ARGS 3) 
(PUT 'MKHASH 'DEFINED-ON-LINE '298) 
(PUT 'MKHASH 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'MKHASH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKHASH (SIZE TYPE EXPANSION) (CONS TYPE NIL)) 
(PUT 'CLRHASH 'NUMBER-OF-ARGS 1) 
(PUT 'CLRHASH 'DEFINED-ON-LINE '301) 
(PUT 'CLRHASH 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'CLRHASH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLRHASH (U) (RPLACD U NIL)) 
(PUT 'GETHASH 'NUMBER-OF-ARGS 2) 
(PUT 'GETHASH 'DEFINED-ON-LINE '304) 
(PUT 'GETHASH 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'GETHASH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GETHASH (KEY TABLE)
    (PROG ()
      (SETQ TABLE
              (COND ((EQUAL (CAR TABLE) 0) (ATSOC KEY (CDR TABLE)))
                    (T (ASSOC KEY (CDR TABLE)))))
      (COND ((NULL TABLE) (RETURN NIL)) (T (RETURN (CDR TABLE)))))) 
(PUT 'PUTHASH 'NUMBER-OF-ARGS 3) 
(PUT 'PUTHASH 'DEFINED-ON-LINE '314) 
(PUT 'PUTHASH 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'PUTHASH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PUTHASH (KEY TABLE VAL)
    (PROG (W)
      (SETQ W
              (COND ((EQUAL (CAR TABLE) 0) (ATSOC KEY (CDR TABLE)))
                    (T (ASSOC KEY (CDR TABLE)))))
      (COND (W (PROGN (RPLACD W VAL) (RETURN VAL))))
      (RPLACD TABLE (CONS (CONS KEY VAL) (CDR TABLE)))
      (RETURN VAL))) 
(PUT 'HASHCONTENTS 'NUMBER-OF-ARGS 1) 
(PUT 'HASHCONTENTS 'DEFINED-ON-LINE '326) 
(PUT 'HASHCONTENTS 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'HASHCONTENTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE HASHCONTENTS (TABLE) (CDR TABLE)) 
(FLAG '(MKHASH) 'RLISP) 
(PUT 'READ_TYPE 'NUMBER-OF-ARGS 0) 
(PUT 'READ_TYPE 'DEFINED-ON-LINE '339) 
(PUT 'READ_TYPE 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'READ_TYPE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_TYPE NIL (PROG (X) (SETQ X CURSYM*) (SCAN) (RETURN X))) 
(PUT 'READ_TYPED_NAME 'NUMBER-OF-ARGS 0) 
(PUT 'READ_TYPED_NAME 'DEFINED-ON-LINE '350) 
(PUT 'READ_TYPED_NAME 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'READ_TYPED_NAME 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_TYPED_NAME NIL
    (PROG (A)
      (SETQ A CURSYM*)
      (SCAN)
      (COND ((NOT (EQUAL CURSYM* '*COLON*)) (RETURN (CONS A 'GENERAL))))
      (SCAN)
      (RETURN (CONS A (READ_TYPE))))) 
(PUT 'READ_NAMELIST 'NUMBER-OF-ARGS 0) 
(PUT 'READ_NAMELIST 'DEFINED-ON-LINE '364) 
(PUT 'READ_NAMELIST 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'READ_NAMELIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_NAMELIST NIL
    (PROG (A)
      (COND ((NOT (VALID_AS_VARIABLE CURSYM*)) (RETURN NIL)))
      (SETQ A (READ_TYPED_NAME))
      (COND ((NOT (EQUAL CURSYM* '*COMMA*)) (RETURN (LIST A))))
      (SCAN)
      (RETURN (CONS A (READ_NAMELIST))))) 
(PUT 'VALID_AS_VARIABLE 'NUMBER-OF-ARGS 1) 
(PUT 'VALID_AS_VARIABLE 'DEFINED-ON-LINE '378) 
(PUT 'VALID_AS_VARIABLE 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'VALID_AS_VARIABLE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE VALID_AS_VARIABLE (U) (AND (IDP U) (NOT (FLAGP U 'INVALID_AS_VARIABLE)))) 
(FLAG '(NIL T *COMMA* *LPAR* *RPAR* *COLON* *SEMICOL*) 'INVALID_AS_VARIABLE) 
(PUT 'READ_SIGNATURE 'NUMBER-OF-ARGS 0) 
(PUT 'READ_SIGNATURE 'DEFINED-ON-LINE '409) 
(PUT 'READ_SIGNATURE 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'READ_SIGNATURE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_SIGNATURE NIL
    (PROG (X Y)
      (SETQ X CURSYM*)
      (COND
       ((NOT (VALID_AS_VARIABLE X))
        (RERROR 'RLISP 7 (LIST X "invalid as formal parameter name"))))
      (SCAN)
      (COND ((EQUAL CURSYM* '*SEMICOL*) (RETURN (LIST (LIST X) 'GENERAL))))
      (COND
       ((EQUAL CURSYM* '*COLON*)
        (PROGN
         (SCAN)
         (SETQ Y (READ_TYPE))
         (COND ((EQUAL CURSYM* '*SEMICOL*) (RETURN (LIST (LIST X) Y)))
               ((OR (NOT (IDP CURSYM*)) (NOT (GET CURSYM* 'INFIX)))
                (SYMERR NIL CURSYM*)))
         (SETQ X (CONS X Y))
         (SETQ Y CURSYM*)
         (SCAN)
         (SETQ X (LIST Y X (READ_TYPED_NAME)))))
       ((EQUAL CURSYM* '*LPAR*)
        (PROGN
         (SCAN)
         (COND ((EQUAL CURSYM* '*RPAR*) (SETQ X (LIST X)))
               (T
                (PROGN
                 (SETQ X (CONS X (READ_NAMELIST)))
                 (COND
                  ((NOT (EQUAL CURSYM* '*RPAR*))
                   (RERROR 'RLISP 8
                           (LIST CURSYM*
                                 "found where right parenthesis expected")))))))
         (SCAN)))
       ((AND (IDP CURSYM*) (GET CURSYM* 'INFIX))
        (PROGN
         (SETQ Y CURSYM*)
         (SCAN)
         (SETQ X (LIST Y (CONS X 'GENERAL) (READ_TYPED_NAME)))))
       (T (SETQ X (LIST X (READ_TYPED_NAME)))))
      (COND
       ((EQUAL CURSYM* '*COLON*) (PROGN (SCAN) (RETURN (LIST X (READ_TYPE)))))
       (T (RETURN (LIST X 'GENERAL)))))) 
(PUT 'MAKE_TUPLE_TYPE 'NUMBER-OF-ARGS 1) 
(PUT 'MAKE_TUPLE_TYPE 'DEFINED-ON-LINE '456) 
(PUT 'MAKE_TUPLE_TYPE 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'MAKE_TUPLE_TYPE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKE_TUPLE_TYPE (X)
    (COND ((NULL X) 'UNIT) ((NULL (CDR X)) (CDAR X))
          (T (CONS 'TIMES (COLLECT_CDRS X))))) 
(PUT 'COLLECT_CARS 'NUMBER-OF-ARGS 1) 
(PUT 'COLLECT_CARS 'DEFINED-ON-LINE '464) 
(PUT 'COLLECT_CARS 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'COLLECT_CARS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COLLECT_CARS (U)
    (COND ((NULL U) NIL) (T (CONS (CAAR U) (COLLECT_CARS (CDR U)))))) 
(PUT 'COLLECT_CDRS 'NUMBER-OF-ARGS 1) 
(PUT 'COLLECT_CDRS 'DEFINED-ON-LINE '468) 
(PUT 'COLLECT_CDRS 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'COLLECT_CDRS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COLLECT_CDRS (U)
    (COND ((NULL U) NIL) (T (CONS (CDAR U) (COLLECT_CDRS (CDR U)))))) 
(PUT 'PROCSTAT1 'NUMBER-OF-ARGS 1) 
(PUT 'PROCSTAT1 'DEFINED-ON-LINE '472) 
(PUT 'PROCSTAT1 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'PROCSTAT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PROCSTAT1 (MODE)
    (PROG (BOOL U TYPE X Y Z FILE LINE INFO)
      (SETQ LINE CURLINE*)
      (SETQ FILE (COND (IFL* (CAR IFL*)) (T "-")))
      (SETQ INFO
              (LIST (CONS 'DEFINED-IN-FILE (INTERN (SIMPLIFY-FILENAME FILE)))
                    (CONS 'DEFINED-ON-LINE LINE)))
      (SETQ BOOL ERFG*)
      (COND
       (FNAME* (PROGN (SETQ BOOL T) (ERRORSET* '(SYMERR 'PROCEDURE T) NIL)))
       (T
        (PROGN
         (COND ((EQUAL CURSYM* 'PROCEDURE) (SETQ TYPE 'EXPR))
               (T (PROGN (SETQ TYPE CURSYM*) (SCAN))))
         (COND
          ((NOT (EQUAL CURSYM* 'PROCEDURE))
           (PROGN (ERRORSET* '(SYMERR 'PROCEDURE T) NIL)))
          (T
           (PROGN
            (COND
             (*REDUCE4
              (PROGN
               (SETQ FNAME* (SCAN))
               (COND ((NOT (IDP FNAME*)) (TYPERR FNAME* "procedure name"))
                     (T
                      (PROGN
                       (SCAN)
                       (SETQ Y
                               (ERRORSET*
                                (LIST 'READ_PARAM_LIST (MKQUOTE MODE)) NIL))
                       (COND
                        ((NOT (ERRORP Y))
                         (PROGN
                          (SETQ Y (CAR Y))
                          (COND
                           ((EQUAL CURSYM* '*COLON*)
                            (SETQ MODE (READ_TYPE))))))))))))
             (T
              (PROGN
               (SCAN)
               (SETQ X (READ_SIGNATURE))
               (SETQ INFO
                       (CONS
                        (CONS 'PROCEDURE_TYPE
                              (CONS 'ARROW
                                    (CONS (MAKE_TUPLE_TYPE (CDAR X)) (CDR X))))
                        INFO))
               (SETQ X (CAR X))
               (SETQ FNAME* (CAR X))
               (SETQ X (CONS FNAME* (COLLECT_CARS (CDR X))))
               (SETQ Y (CDR X)))))))))))
      (COND
       ((GREATERP EOF* 0)
        (PROGN (SETQ CURSYM* '*SEMICOL*) (SETQ CURESCAPED* NIL)))
       (T
        (PROGN
         (SETQ Z (ERRORSET* '(XREAD T) NIL))
         (COND ((NOT (ERRORP Z)) (SETQ Z (CAR Z))))
         (COND
          ((NULL ERFG*)
           (SETQ Z
                   (LIST 'PROCEDURE (COND ((NULL *REDUCE4) (CAR X)) (T FNAME*))
                         MODE TYPE Y Z INFO)))))))
      (REMFLAG (LIST FNAME*) 'FNC)
      (SETQ FNAME* NIL)
      (COND (ERFG* (PROGN (SETQ Z NIL) (COND ((NOT BOOL) (ERROR1))))))
      (RETURN Z))) 
(PUT 'PROCSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'PROCSTAT 'DEFINED-ON-LINE '564) 
(PUT 'PROCSTAT 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'PROCSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PROCSTAT NIL (PROCSTAT1 NIL)) 
(PUT 'LISTPROCIFY 'NUMBER-OF-ARGS 1) 
(PUT 'LISTPROCIFY 'DEFINED-ON-LINE '570) 
(PUT 'LISTPROCIFY 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'LISTPROCIFY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISTPROCIFY (U)
    (COND ((ATOM U) U) ((EQCAR U 'PROCEDURE) (CONS 'LISTPROC U))
          (T
           (CONS (CAR U)
                 (PROG (A FORALL-RESULT FORALL-ENDPTR)
                   (SETQ A (CDR U))
                   (COND ((NULL A) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (A) (LISTPROCIFY A)) (CAR A))
                                         NIL)))
                  LOOPLABEL
                   (SETQ A (CDR A))
                   (COND ((NULL A) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (A) (LISTPROCIFY A)) (CAR A)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))) 
(PUT 'READLISTPROC 'NUMBER-OF-ARGS 0) 
(PUT 'READLISTPROC 'DEFINED-ON-LINE '575) 
(PUT 'READLISTPROC 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'READLISTPROC 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READLISTPROC NIL
    (PROG (W)
      (SETQ CURSYM* 'PROCEDURE)
      (SETQ W (PROCSTAT1 'ALGEBRAIC))
      (RETURN (LISTPROCIFY W)))) 
(PUT 'FORMLISTPROC 'NUMBER-OF-ARGS 3) 
(PUT 'FORMLISTPROC 'DEFINED-ON-LINE '587) 
(PUT 'FORMLISTPROC 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'FORMLISTPROC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMLISTPROC (U V W)
    (PROG ()
      (RETURN
       (LIST 'PROGN (FORMPROC (CDR U) V W)
             (LIST 'PUT (MKQUOTE (CADDR U)) ''RTYPEFN ''(LAMBDA (X) 'LIST))
             (LIST 'PUT (MKQUOTE (CADDR U)) ''LISTFN
                   (LIST 'LIST ''LAMBDA ''(X Y)
                         (LIST 'LIST ''LISTPROCEVAL
                               (MKQUOTE (MKQUOTE (CADDR U))) ''X ''Y)))
             (LIST 'REMFLAG (LIST 'LIST (MKQUOTE (CADDR U))) ''OPFN)
             (MKQUOTE (CADDR U)))))) 
(PUT 'LISTPROC 'FORMFN 'FORMLISTPROC) 
(PUT 'LISTPROCEVAL 'NUMBER-OF-ARGS 3) 
(PUT 'LISTPROCEVAL 'DEFINED-ON-LINE '601) 
(PUT 'LISTPROCEVAL 'DEFINED-IN-FILE 'RLISP/PROC.RED) 
(PUT 'LISTPROCEVAL 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LISTPROCEVAL (OP U V) (REVAL1 (OPFNEVAL (CONS OP U)) V)) 
(DEFLIST
 '((PROCEDURE PROCSTAT) (EXPR PROCSTAT) (FEXPR PROCSTAT) (EMB PROCSTAT)
   (MACRO PROCSTAT) (INLINE PROCSTAT) (SMACRO PROCSTAT)
   (LISTPROC READLISTPROC))
 'STAT) 
(COND ((EQUAL (GET 'SYMBOLIC 'STAT) 'PROCSTAT) (REMPROP 'SYMBOLIC 'STAT))) 
(DEFLIST '((LISP SYMBOLIC)) 'NEWNAM) 
(ENDMODULE) 