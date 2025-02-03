(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ASSERTPROC)) 
(PUT 'ASSERT_PROCSTAT 'NUMBER-OF-ARGS 0) 
(PUT 'ASSERT_PROCSTAT 'DEFINED-ON-LINE '30) 
(PUT 'ASSERT_PROCSTAT 'DEFINED-IN-FILE 'ASSERT/ASSERTPROC.RED) 
(PUT 'ASSERT_PROCSTAT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ASSERT_PROCSTAT NIL
    (PROG (FTYPE FNAME W BODY)
      (SCAN)
      (COND ((EQ CURSYM* 'INLINE) (PROGN (SETQ FTYPE 'INLINE) (SCAN)))
            (T (SETQ FTYPE 'EXPR)))
      (COND
       ((NEQ CURSYM* 'PROCEDURE)
        (ASSERT_REDERR
         (LIST "expecting keyword procedure but found" CURSYM*))))
      (SCAN)
      (SETQ FNAME CURSYM*)
      (COND
       ((NOT (IDP CURSYM*))
        (ASSERT_REDERR (LIST "expecting procedure name but found" CURSYM*))))
      (SCAN)
      (COND
       ((NEQ CURSYM* '*LPAR*)
        (ASSERT_REDERR (LIST "expecting '(' but found" CURSYM*))))
      (SCAN)
      (SETQ W (ASSERT_PROCSTAT-ARGL))
      (SCAN)
      (COND
       ((NEQ CURSYM* '*SEMICOL*)
        (ASSERT_REDERR (LIST "expecting ';' but found" CURSYM*))))
      (SETQ BODY (XREAD T))
      (RETURN (LIST 'ASSERT_PROCEDURE FNAME FTYPE W BODY)))) 
(PUT 'ASSERT_PROCSTAT-ARGL 'NUMBER-OF-ARGS 0) 
(PUT 'ASSERT_PROCSTAT-ARGL 'DEFINED-ON-LINE '56) 
(PUT 'ASSERT_PROCSTAT-ARGL 'DEFINED-IN-FILE 'ASSERT/ASSERTPROC.RED) 
(PUT 'ASSERT_PROCSTAT-ARGL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ASSERT_PROCSTAT-ARGL NIL
    (PROG (VAR TYPE W ARGTYPEL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ CURSYM* '*RPAR*)) (RETURN NIL)))
        (PROGN
         (EOLCHECK)
         (COND
          ((NOT (IDP CURSYM*))
           (ASSERT_REDERR (LIST "Expecting identifier but found" CURSYM*))))
         (SETQ VAR CURSYM*)
         (SCAN)
         (SETQ TYPE
                 (COND
                  ((EQ CURSYM* '*COLON*)
                   (PROGN
                    (SCAN)
                    (COND
                     ((NOT (ASSERT_TYPESYNTAXP CURSYM*))
                      (ASSERT_REDERR
                       (LIST "Expecting type but found" CURSYM*))))
                    (SETQ W CURSYM*)
                    (SCAN)
                    W))))
         (SETQ ARGTYPEL (CONS (CONS VAR TYPE) ARGTYPEL))
         (COND
          ((NOT (MEMQ CURSYM* '(*COMMA* *RPAR*)))
           (ASSERT_REDERR (LIST "Expecting ',' or ')' but found" CURSYM*))))
         (COND ((EQ CURSYM* '*COMMA*) (SCAN)))
         NIL)
        (GO WHILELABEL))
      (SETQ TYPE
              (COND
               ((OR (EQ NXTSYM* '*COLON*) (EQ NXTSYM* '|:|))
                (PROGN
                 (SCAN)
                 (SCAN)
                 (COND
                  ((NOT (ASSERT_TYPESYNTAXP CURSYM*))
                   (ASSERT_REDERR (LIST "Expecting type but found" CURSYM*))))
                 CURSYM*))))
      (SETQ ARGTYPEL (CONS (CONS 'RETURNVALUE TYPE) ARGTYPEL))
      (SETQ ARGTYPEL (REVERSIP ARGTYPEL))
      (RETURN ARGTYPEL))) 
(PUT 'ASSERT_TYPESYNTAXP 'NUMBER-OF-ARGS 1) 
(PUT 'ASSERT_TYPESYNTAXP 'DEFINED-ON-LINE '90) 
(PUT 'ASSERT_TYPESYNTAXP 'DEFINED-IN-FILE 'ASSERT/ASSERTPROC.RED) 
(PUT 'ASSERT_TYPESYNTAXP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ASSERT_TYPESYNTAXP (S) (COND (*ASSERT (ASSERT_DYNTYPEP S)) (T (IDP S)))) 
(PUT 'ASSERT_FORMPROC 'NUMBER-OF-ARGS 3) 
(PUT 'ASSERT_FORMPROC 'DEFINED-ON-LINE '93) 
(PUT 'ASSERT_FORMPROC 'DEFINED-IN-FILE 'ASSERT/ASSERTPROC.RED) 
(PUT 'ASSERT_FORMPROC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE ASSERT_FORMPROC (U VARS MODE)
    (PROG (FNAME FTYPE TARGL BODY ARGL ATYPEL RTYPE FPC ASSRT)
      (COND
       ((NEQ MODE 'SYMBOLIC)
        (REDERR
         (LIST "asserted procedures are available in symbolic mode only"))))
      (SETQ FNAME (CADR U))
      (SETQ FTYPE (CADDR U))
      (SETQ TARGL (CADDDR U))
      (SETQ BODY (CAR (CDDDDR U)))
      (SETQ TARGL (REVERSE TARGL))
      (SETQ RTYPE (CDR (CAR TARGL)))
      (SETQ TARGL (CDR TARGL))
      (PROG (PR)
        (SETQ PR TARGL)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (SETQ ARGL (CONS (CAR PR) ARGL))
            (SETQ ATYPEL (CONS (CDR PR) ATYPEL))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (COND
       ((NOT
         (AND *ASSERT *ASSERT_PROCEDURES
              (OR *ASSERT_INLINE_PROCEDURES (NEQ FTYPE 'INLINE))))
        (RETURN
         (FORMPROC (LIST 'PROCEDURE FNAME NIL FTYPE ARGL BODY) VARS MODE))))
      (SETQ FPC
              (FORMPROC (LIST 'PROCEDURE FNAME NIL 'EXPR ARGL BODY) VARS MODE))
      (SETQ ASSRT (ASSERT_DECLARESTAT1 (LIST FNAME ATYPEL RTYPE)))
      (RETURN (LIST 'PROGN FPC ASSRT)))) 
(PUT 'ASSERT_REDERR 'NUMBER-OF-ARGS 1) 
(PUT 'ASSERT_REDERR 'DEFINED-ON-LINE '116) 
(PUT 'ASSERT_REDERR 'DEFINED-IN-FILE 'ASSERT/ASSERTPROC.RED) 
(PUT 'ASSERT_REDERR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ASSERT_REDERR (L)
    (PROGN
     (PROG ()
      WHILELABEL
       (COND ((NOT (NEQ CURSYM* '*SEMICOL*)) (RETURN NIL)))
       (SCAN)
       (GO WHILELABEL))
     (XREAD T)
     (REDERR L))) 
(PUT 'ASSERTED 'STAT 'ASSERT_PROCSTAT) 
(PUT 'ASSERT_PROCEDURE 'FORMFN 'ASSERT_FORMPROC) 
(ENDMODULE) 