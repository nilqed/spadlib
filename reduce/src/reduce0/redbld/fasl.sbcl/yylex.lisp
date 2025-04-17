(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'YYLEX)) 
(FLUID
 '(LEXER_STYLE* LEXER_STYLE_RLISP LEXER_STYLE_C LEXER_STYLE_SML
   LEXER_STYLE_SCRIPT LEXER_STYLE_TEX)) 
(SETQ LEXER_STYLE_RLISP (PLUS 1 64 128 1024 16384 32768 262144)) 
(SETQ LEXER_STYLE_C (PLUS 4 8 256 128 512 2048 16384)) 
(SETQ LEXER_STYLE_SML (PLUS 16 32 128 512 2048 16384 524288 8192 65536 131072)) 
(SETQ LEXER_STYLE_SCRIPT (PLUS 2 128 512 2048 16384)) 
(SETQ LEXER_STYLE_TEX (PLUS 1 4096)) 
(SETQ LEXER_STYLE* LEXER_STYLE_RLISP) 
(DE LEXER_OPTION (O) (NOT (ZEROP (LAND LEXER_STYLE* O)))) 
(PUT 'LEXER_OPTION 'NUMBER-OF-ARGS 1) 
(PUT 'LEXER_OPTION 'DEFINED-ON-LINE '257) 
(PUT 'LEXER_OPTION 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEXER_OPTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'LEXER_OPTION 'INLINE '(LAMBDA (O) (NOT (ZEROP (LAND LEXER_STYLE* O))))) 
(FLAG '(! % & $ |#| + - / |:| < = > ? @ |\\| ~ |`| ^ |\|| *) 'SML_OPCHAR) 
(DE SML_OPCHAR (CH)
    (AND (NOT (ZEROP (LAND LEXER_STYLE* 131072))) (FLAGP CH 'SML_OPCHAR))) 
(PUT 'SML_OPCHAR 'NUMBER-OF-ARGS 1) 
(PUT 'SML_OPCHAR 'DEFINED-ON-LINE '266) 
(PUT 'SML_OPCHAR 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'SML_OPCHAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'SML_OPCHAR 'INLINE
      '(LAMBDA (CH)
         (AND (NOT (ZEROP (LAND LEXER_STYLE* 131072))) (FLAGP CH 'SML_OPCHAR)))) 
(PUT 'ALL_SML_OPCHAR 'NUMBER-OF-ARGS 1) 
(PUT 'ALL_SML_OPCHAR 'DEFINED-ON-LINE '270) 
(PUT 'ALL_SML_OPCHAR 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'ALL_SML_OPCHAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALL_SML_OPCHAR (L)
    (OR (NULL L)
        (AND
         (AND (NOT (ZEROP (LAND LEXER_STYLE* 131072)))
              (FLAGP (CAR L) 'SML_OPCHAR))
         (ALL_SML_OPCHAR (CDR L))))) 
(GLOBAL '(LEX_KEYWORD_NAMES LEX_NEXT_CODE LEX_INITIAL_NEXT_CODE LEX_CODENAME)) 
(GLOBAL '(INITIAL_CODENAME)) 
(SETQ INITIAL_CODENAME
        '((0 . |:EOF|) (1 . |:SYMBOL|) (2 . |:TYPENAME|) (3 . |:STRING|)
          (4 . |:CHAR|) (5 . |:NUMBER|) (6 . |:LIST|) (7 . |:INFIX0|)
          (8 . |:INFIX1|) (9 . |:INFIX2|) (10 . |:INFIX3|) (11 . |:INFIX4|)
          (12 . |:INFIX5|) (13 . |:INFIX6|) (14 . |:INFIX7|) (15 . |:INFIX8|)
          (16 . |:INFIX9|) (17 . |:INFIXR0|) (18 . |:INFIXR1|)
          (19 . |:INFIXR2|) (20 . |:INFIXR3|) (21 . |:INFIXR4|)
          (22 . |:INFIXR5|) (23 . |:INFIXR6|) (24 . |:INFIXR7|)
          (25 . |:INFIXR8|) (26 . |:INFIXR9|))) 
(PROG (P)
  (SETQ P INITIAL_CODENAME)
 LAB
  (COND ((NULL P) (RETURN NIL)))
  ((LAMBDA (P) (PUT (CDR P) 'LEX_FIXED_CODE (CAR P))) (CAR P))
  (SETQ P (CDR P))
  (GO LAB)) 
(SETQ LEX_CODENAME INITIAL_CODENAME) 
(SETQ LEX_INITIAL_NEXT_CODE
        (SETQ LEX_NEXT_CODE (PLUS 1 (CAAR (REVERSE INITIAL_CODENAME))))) 
(SETQ LEX_KEYWORD_NAMES NIL) 
(GLOBAL
 '(LEX_ESCAPED LEX_EOF_CODE LEX_SYMBOL_CODE LEX_TYPENAME_CODE LEX_NUMBER_CODE
   LEX_STRING_CODE LEX_CHAR_CODE LEX_LIST_CODE)) 
(SETQ LEX_EOF_CODE (GET '|:EOF| 'LEX_FIXED_CODE)) 
(SETQ LEX_SYMBOL_CODE (GET '|:SYMBOL| 'LEX_FIXED_CODE)) 
(SETQ LEX_TYPENAME_CODE (GET '|:TYPENAME| 'LEX_FIXED_CODE)) 
(SETQ LEX_NUMBER_CODE (GET '|:NUMBER| 'LEX_FIXED_CODE)) 
(SETQ LEX_STRING_CODE (GET '|:STRING| 'LEX_FIXED_CODE)) 
(SETQ LEX_CHAR_CODE (GET '|:CHAR| 'LEX_FIXED_CODE)) 
(SETQ LEX_LIST_CODE (GET '|:LIST| 'LEX_FIXED_CODE)) 
(DE LEX_UNICODE_ALPHABETIC (C)
    (OR (AND (GEQ C 65) (LEQ C 90)) (AND (GEQ C 97) (LEQ C 122)))) 
(PUT 'LEX_UNICODE_ALPHABETIC 'NUMBER-OF-ARGS 1) 
(PUT 'LEX_UNICODE_ALPHABETIC 'DEFINED-ON-LINE '411) 
(PUT 'LEX_UNICODE_ALPHABETIC 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_UNICODE_ALPHABETIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'LEX_UNICODE_ALPHABETIC 'INLINE
      '(LAMBDA (C)
         (OR (AND (GEQ C 65) (LEQ C 90)) (AND (GEQ C 97) (LEQ C 122))))) 
(DE LEX_UNICODE_NUMERIC (C) (AND (GEQ C 48) (LEQ C 57))) 
(PUT 'LEX_UNICODE_NUMERIC 'NUMBER-OF-ARGS 1) 
(PUT 'LEX_UNICODE_NUMERIC 'DEFINED-ON-LINE '417) 
(PUT 'LEX_UNICODE_NUMERIC 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_UNICODE_NUMERIC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(PUTC 'LEX_UNICODE_NUMERIC 'INLINE '(LAMBDA (C) (AND (GEQ C 48) (LEQ C 57)))) 
(PUT 'LEX_HEXVAL 'NUMBER-OF-ARGS 1) 
(PUT 'LEX_HEXVAL 'DEFINED-ON-LINE '422) 
(PUT 'LEX_HEXVAL 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_HEXVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEX_HEXVAL (C)
    ((LAMBDA (N)
       (COND ((AND (GEQ N 48) (LEQ N 57)) (DIFFERENCE N 48))
             ((AND (GEQ N 65) (LEQ N 70)) (PLUS (DIFFERENCE N 65) 10))
             ((AND (GEQ N 97) (LEQ N 102)) (PLUS (DIFFERENCE N 97) 10))
             (T NIL)))
     (CAR (WIDESTRING2LIST (SYMBOL-NAME C))))) 
(PUT 'LEX_KEYWORDS 'NUMBER-OF-ARGS 1) 
(PUT 'LEX_KEYWORDS 'DEFINED-ON-LINE '428) 
(PUT 'LEX_KEYWORDS 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_KEYWORDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEX_KEYWORDS (L)
    (PROG (X)
      (SETQ X L)
     LAB
      (COND ((NULL X) (RETURN NIL)))
      ((LAMBDA (X)
         (PROG (W OK PRE)
           (SETQ W (WIDESTRING2LIST X))
           (COND ((NULL W) (REDERR "Empty string passed to lex_keywords")))
           (SETQ OK
                   (OR (AND (GEQ (CAR W) 65) (LEQ (CAR W) 90))
                       (AND (GEQ (CAR W) 97) (LEQ (CAR W) 122))))
           (PROG (C)
             (SETQ C (CDR W))
            LAB
             (COND ((NULL C) (RETURN NIL)))
             ((LAMBDA (C)
                (COND
                 ((AND
                   (NOT
                    (OR (AND (GEQ C 65) (LEQ C 90))
                        (AND (GEQ C 97) (LEQ C 122))))
                   (NOT (AND (GEQ C 48) (LEQ C 57))) (NEQ C 95))
                  (SETQ OK NIL))))
              (CAR C))
             (SETQ C (CDR C))
             (GO LAB))
           (COND
            ((OR (NULL (CDR W)) OK)
             (PROGN
              (SETQ W (INTERN X))
              (COND
               ((NULL (GET W 'LEX_CODE))
                (PROGN
                 (SETQ LEX_KEYWORD_NAMES (CONS W LEX_KEYWORD_NAMES))
                 (COND
                  ((NULL (GET W 'LEX_NEXT_CODE))
                   (PROGN
                    (PUT W 'LEX_CODE LEX_NEXT_CODE)
                    (SETQ LEX_CODENAME
                            (CONS (CONS LEX_NEXT_CODE W) LEX_CODENAME))
                    (COND
                     (*TRACELEX
                      (PROGN
                       (PRINC "Token '")
                       (PRIN1 W)
                       (PRINC "' allocated code ")
                       (PRINT LEX_NEXT_CODE))))
                    (SETQ LEX_NEXT_CODE (PLUS LEX_NEXT_CODE 1))))))))
              (RETURN NIL))))
           (SETQ X (INTERN X))
           (COND
            ((NOT (GET X 'LEX_CODE))
             (PROGN
              (SETQ LEX_KEYWORD_NAMES (CONS X LEX_KEYWORD_NAMES))
              (PUT X 'LEX_CODE LEX_NEXT_CODE)
              (SETQ LEX_CODENAME (CONS (CONS LEX_NEXT_CODE X) LEX_CODENAME))
              (SETQ LEX_NEXT_CODE (PLUS LEX_NEXT_CODE 1))
              (SETQ PRE (LIST2WIDESTRING (REVERSE (CDR (REVERSE W)))))
              (LEX_KEYWORDS (LIST PRE))
              (SETQ PRE (INTERN PRE))
              (SETQ W (INTERN (LIST2WIDESTRING (LIST (LASTCAR W)))))
              (COND
               (*TRACELEX
                (PROGN
                 (COND ((NOT (ZEROP (POSN))) (TERPRI)))
                 (PRINC "dipthong data '")
                 (PRIN1 PRE)
                 (PRINC "' plus '")
                 (PRIN1 W)
                 (PRINC "' => '")
                 (PRIN1 X)
                 (PROGN (PRIN2 "'") (TERPRI) "'"))))
              (PUT PRE 'LEX_DIPTHONG
                   (CONS (CONS W X) (GET PRE 'LEX_DIPTHONG))))))))
       (CAR X))
      (SETQ X (CDR X))
      (GO LAB))) 
(PUT 'LEX_CLEANUP 'NUMBER-OF-ARGS 0) 
(PUT 'LEX_CLEANUP 'DEFINED-ON-LINE '485) 
(PUT 'LEX_CLEANUP 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_CLEANUP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LEX_CLEANUP NIL
    (PROG ()
      (PROG (X)
        (SETQ X LEX_KEYWORD_NAMES)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (PROGN (REMPROP X 'LEX_CODE) (REMPROP X 'LEX_DIPTHONG)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ LEX_KEYWORD_NAMES NIL)
      (SETQ LEX_NEXT_CODE LEX_INITIAL_NEXT_CODE)
      (SETQ LEX_CODENAME INITIAL_CODENAME))) 
(PUT 'LEX_SAVE_CONTEXT 'NUMBER-OF-ARGS 0) 
(PUT 'LEX_SAVE_CONTEXT 'DEFINED-ON-LINE '504) 
(PUT 'LEX_SAVE_CONTEXT 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_SAVE_CONTEXT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LEX_SAVE_CONTEXT NIL
    (PROG (W FORALL-RESULT FORALL-ENDPTR)
      (SETQ W LEX_CODENAME)
      (COND ((NULL W) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (W)
                          (CONS (GET (INTERN (CDR W)) 'LEX_DIPTHONG) W))
                        (CAR W))
                       NIL)))
     LOOPLABEL
      (SETQ W (CDR W))
      (COND ((NULL W) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (W) (CONS (GET (INTERN (CDR W)) 'LEX_DIPTHONG) W))
                (CAR W))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'LEX_RESTORE_CONTEXT 'NUMBER-OF-ARGS 1) 
(PUT 'LEX_RESTORE_CONTEXT 'DEFINED-ON-LINE '507) 
(PUT 'LEX_RESTORE_CONTEXT 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_RESTORE_CONTEXT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEX_RESTORE_CONTEXT (CONTEXT)
    (PROG (TOKEN DIPTHONG CODE)
      (LEX_CLEANUP)
      (PROG (X)
        (SETQ X CONTEXT)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ DIPTHONG (CAR X))
            (SETQ CODE (CADR X))
            (SETQ TOKEN (INTERN (CDDR X)))
            (COND
             ((NOT (GET TOKEN 'LEX_FIXED_CODE))
              (PROGN
               (COND ((GREATERP CODE LEX_NEXT_CODE) (SETQ LEX_NEXT_CODE CODE)))
               (PUT TOKEN 'LEX_DIPTHONG DIPTHONG)
               (PUT TOKEN 'LEX_CODE CODE)
               (SETQ LEX_CODENAME (CONS (CONS CODE TOKEN) LEX_CODENAME))
               (SETQ LEX_KEYWORD_NAMES (CONS TOKEN LEX_KEYWORD_NAMES)))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB)))) 
(PUT 'LEX_EXPORT_CODES 'NUMBER-OF-ARGS 0) 
(PUT 'LEX_EXPORT_CODES 'DEFINED-ON-LINE '529) 
(PUT 'LEX_EXPORT_CODES 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_EXPORT_CODES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LEX_EXPORT_CODES NIL (SORT LEX_CODENAME (FUNCTION ORDOPCAR))) 
(FLUID '(LEX_CHAR LEX_PEEK_CHAR)) 
(SETQ LEX_PEEK_CHAR NIL) 
(PUT 'YYREADCH 'NUMBER-OF-ARGS 0) 
(PUT 'YYREADCH 'DEFINED-ON-LINE '543) 
(PUT 'YYREADCH 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'YYREADCH 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE YYREADCH NIL
    (COND
     ((NOT (NULL LEX_PEEK_CHAR))
      (PROGN (SETQ LEX_CHAR LEX_PEEK_CHAR) (SETQ LEX_PEEK_CHAR NIL) LEX_CHAR))
     (T
      (PROGN
       (SETQ LEX_CHAR (READCH))
       (COND ((EQUAL LEX_CHAR $EOL$) (SETQ WHICH_LINE (PLUS WHICH_LINE 1))))
       (COND
        ((NEQ LEX_CHAR $EOF$)
         (PROGN
          (SETQ LAST64P (PLUS LAST64P 1))
          (COND ((EQUAL LAST64P 64) (SETQ LAST64P 0)))
          (PUTV LAST64 LAST64P LEX_CHAR))))
       LEX_CHAR)))) 
(PUT 'YYPEEK 'NUMBER-OF-ARGS 0) 
(PUT 'YYPEEK 'DEFINED-ON-LINE '559) 
(PUT 'YYPEEK 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'YYPEEK 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE YYPEEK NIL
    (PROG (LEX_CHAR)
      (COND ((NULL LEX_PEEK_CHAR) (SETQ LEX_PEEK_CHAR (YYREADCH))))
      (RETURN LEX_PEEK_CHAR))) 
(SWITCH (LIST 'PARSE_ERRORS_FATAL)) 
(PUT 'YYERROR 'NUMBER-OF-ARGS 1) 
(PUT 'YYERROR 'DEFINED-ON-LINE '568) 
(PUT 'YYERROR 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'YYERROR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE YYERROR (MSG)
    (PROG (C)
      (TERPRI)
      (PRIN2 "+++++ Parse error at line ")
      (PRIN1 WHICH_LINE)
      (PRIN2 ":")
      (COND ((ATOM MSG) (SETQ MSG (LIST MSG))))
      (PROG (S)
        (SETQ S MSG)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S) (PROGN (PRIN2 " ") (PRIN2 S))) (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (TERPRI)
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE 64 I)) (RETURN NIL)))
        (PROGN
         (SETQ LAST64P (PLUS LAST64P 1))
         (COND ((EQUAL LAST64P 64) (SETQ LAST64P 0)))
         (SETQ C (GETV LAST64 LAST64P))
         (COND ((NOT (EQUAL C NIL)) (PRIN2 C))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PRINC "^^^")
      (COND ((NOT (EQUAL C $EOL$)) (TERPRI)))
      (COND ((EQUAL LEX_CHAR $EOF$) (PROGN (PRIN2 "<EOF>") (TERPRI) "<EOF>")))
      (COND
       (*PARSE_ERRORS_FATAL
        (PROGN
         (COND ((NOT (ZEROP (POSN))) (TERPRI)))
         (PROGN
          (PRIN2 "+++ Quitting (parse_errors_fatal is set)")
          (TERPRI)
          "+++ Quitting (parse_errors_fatal is set)")
         (BYE)))))) 
(GLOBAL '(LEX_PEEKED)) 
(PUT 'LEX_INIT 'NUMBER-OF-ARGS 0) 
(PUT 'LEX_INIT 'DEFINED-ON-LINE '596) 
(PUT 'LEX_INIT 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_INIT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LEX_INIT NIL
    (PROGN
     (SETQ LAST64 (MKVECT 64))
     (SETQ LAST64P 0)
     (SETQ WHICH_LINE 1)
     (SETQ IF_DEPTH 0)
     (COND
      (*TRACELEX
       (PROGN
        (COND ((NEQ (POSN) 0) (TERPRI)))
        (PROGN (PRIN2 "yylex initialized") (TERPRI) "yylex initialized"))))
     (SETQ LEX_PEEKED NIL)
     (YYREADCH))) 
(PUT 'LEX_PROCESS_DIRECTIVE 'NUMBER-OF-ARGS 0) 
(PUT 'LEX_PROCESS_DIRECTIVE 'DEFINED-ON-LINE '681) 
(PUT 'LEX_PROCESS_DIRECTIVE 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_PROCESS_DIRECTIVE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LEX_PROCESS_DIRECTIVE NIL
    (PROG (W)
      (COND
       ((EQUAL YYLVAL '|#ENDIF|)
        (PROGN
         (COND
          ((ZEROP IF_DEPTH)
           (PROGN
            (PRIN2 "+++ #endif not follopwing #if")
            (TERPRI)
            "+++ #endif not follopwing #if")))
         (SETQ IF_DEPTH (DIFFERENCE IF_DEPTH 1))
         (RETURN T)))
       ((EQUAL YYLVAL $EOF$)
        (PROGN
         (COND
          ((NOT (ZEROP IF_DEPTH))
           (PROGN
            (PRIN2 "+++ #endif missing at end of file")
            (TERPRI)
            "+++ #endif missing at end of file")))
         (RETURN T)))
       ((EQUAL YYLVAL '|#IF|)
        (PROGN
         (SETQ IF_DEPTH (PLUS IF_DEPTH 1))
         (READ_S_EXPRESSION)
         (SETQ W (ERRORSET YYLVAL NIL NIL))
         (COND
          ((OR (ERRORP W) (NULL (CAR W)))
           (RETURN (LEX_SKIP_TO_ELSE_OR_ENDIF T)))
          (T (RETURN T)))))
       ((EQUAL YYLVAL '|#ELSE|) (RETURN (LEX_SKIP_TO_ELSE_OR_ENDIF NIL)))
       ((EQUAL YYLVAL '|#ELIF|)
        (PROGN (READ_S_EXPRESSION) (RETURN (LEX_SKIP_TO_ELSE_OR_ENDIF NIL))))
       (T (RETURN NIL))))) 
(PUT 'LEX_IS_ELSE_OR_ENDIF 'NUMBER-OF-ARGS 2) 
(PUT 'LEX_IS_ELSE_OR_ENDIF 'DEFINED-ON-LINE '707) 
(PUT 'LEX_IS_ELSE_OR_ENDIF 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_IS_ELSE_OR_ENDIF 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LEX_IS_ELSE_OR_ENDIF (W ELIF)
    (COND ((EQUAL W LEX_EOF_CODE) T) ((NEQ W LEX_SYMBOL_CODE) NIL)
          (LEX_ESCAPED NIL)
          ((EQUAL YYLVAL '|#ENDIF|)
           (PROGN
            (COND
             ((ZEROP IF_DEPTH)
              (PROGN
               (PRIN2 "+++ #endif without previous #if")
               (TERPRI)
               "+++ #endif without previous #if")))
            T))
          ((NOT ELIF) NIL) ((EQUAL YYLVAL '|#ELSE|) T)
          ((NEQ YYLVAL '|#ELIF|) NIL)
          (T
           (PROGN
            (READ_S_EXPRESSION)
            (SETQ W (ERRORSET YYLVAL NIL NIL))
            (AND (NOT (ERRORP W)) (CAR W)))))) 
(PUT 'LEX_SKIP_TO_ELSE_OR_ENDIF 'NUMBER-OF-ARGS 1) 
(PUT 'LEX_SKIP_TO_ELSE_OR_ENDIF 'DEFINED-ON-LINE '723) 
(PUT 'LEX_SKIP_TO_ELSE_OR_ENDIF 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_SKIP_TO_ELSE_OR_ENDIF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEX_SKIP_TO_ELSE_OR_ENDIF (ELIF)
    (PROG ()
      (COND
       ((ZEROP IF_DEPTH)
        (PROGN
         (PRIN2 "+++ #else of #elif witout preceeding #if")
         (TERPRI)
         "+++ #else of #elif witout preceeding #if")))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (NOT (LEX_IS_ELSE_OR_ENDIF (LEX_BASIC_TOKEN) ELIF)))
          (RETURN NIL)))
        (PROGN
         (COND
          ((AND (EQUAL YYLVAL '|#IF|) (NOT LEX_ESCAPED))
           (PROGN
            (SETQ IF_DEPTH (PLUS IF_DEPTH 1))
            (READ_S_EXPRESSION)
            (LEX_SKIP_TO_ELSE_OR_ENDIF NIL)))
          ((AND (EQUAL YYLVAL '|#DEFINE|) (NOT LEX_ESCAPED))
           (PROGN (READ_S_EXPRESSION) (READ_S_EXPRESSION)))
          ((AND (OR (EQUAL YYLVAL '|#ELIF|) (EQUAL YYLVAL '|#EVAL|))
                (NOT LEX_ESCAPED))
           (READ_S_EXPRESSION))))
        (GO WHILELABEL))
      (RETURN T))) 
(PUT 'YYLEX 'NUMBER-OF-ARGS 0) 
(PUT 'YYLEX 'DEFINED-ON-LINE '773) 
(PUT 'YYLEX 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'YYLEX 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE YYLEX NIL
    (PROG (W W1 DONE)
      (SETQ W (LEX_BASIC_TOKEN))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (NOT DONE) (EQUAL W LEX_SYMBOL_CODE) (NOT LEX_ESCAPED)
                (NOT (ZEROP (LAND LEXER_STYLE* 64)))))
          (RETURN NIL)))
        (PROGN
         (COND ((LEX_PROCESS_DIRECTIVE) (SETQ W (LEX_BASIC_TOKEN)))
               ((EQUAL YYLVAL '|#DEFINE|)
                (PROGN
                 (READ_S_EXPRESSION)
                 (SETQ W YYLVAL)
                 (READ_S_EXPRESSION)
                 (SETQ W1 YYLVAL)
                 (COND
                  ((AND (IDP W) (OR (IDP W1) (NUMBERP W1) (STRINGP W1)))
                   (PROGN
                    (COND ((NOT (ZEROP (POSN))) (TERPRI)))
                    (PRINC "+++ ")
                    (PRIN1 W)
                    (PRINC " => ")
                    (PRINT W1)
                    (PUT 1 '|#DEFINE| (LIST W1)))))
                 (SETQ W (LEX_BASIC_TOKEN))))
               ((EQUAL YYLVAL '|#EVAL|)
                (PROGN
                 (READ_S_EXPRESSION)
                 (ERRORSET YYLVAL NIL NIL)
                 (SETQ W (LEX_BASIC_TOKEN))))
               ((SETQ W1 (GET YYLVAL '|#DEFINE|))
                (PROGN
                 (SETQ YYLVAL (CAR W1))
                 (COND ((NUMBERP W1) (SETQ W LEX_NUMBER_CODE))
                       ((STRINGP W1) (SETQ W LEX_STRING_CODE)))
                 (SETQ DONE T)))
               (T (SETQ DONE T))))
        (GO WHILELABEL))
      (COND
       (*TRACELEX
        (PROGN
         (COND ((NEQ (POSN) 0) (TERPRI)))
         (PRIN2 "yylex = ")
         (PRIN1 YYLVAL)
         (PRIN2 " type ")
         (PRINT W))))
      (RETURN W))) 
(GLOBAL '(LEX_PEEKED LEX_PEEKED_YYLVAL LEX_PEEKED_ESCAPED)) 
(PUT 'LEX_START_LINE_COMMENT 'NUMBER-OF-ARGS 1) 
(PUT 'LEX_START_LINE_COMMENT 'DEFINED-ON-LINE '834) 
(PUT 'LEX_START_LINE_COMMENT 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_START_LINE_COMMENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEX_START_LINE_COMMENT (CH)
    (OR (AND (EQUAL CH '%) (NOT (ZEROP (LAND LEXER_STYLE* 1))))
        (AND (EQUAL CH '|#|) (NOT (ZEROP (LAND LEXER_STYLE* 2))))
        (AND (EQUAL CH '/) (NOT (ZEROP (LAND LEXER_STYLE* 4)))
             (EQUAL (YYPEEK) '/)))) 
(PUT 'LEX_SKIP_LINE_COMMENT 'NUMBER-OF-ARGS 0) 
(PUT 'LEX_SKIP_LINE_COMMENT 'DEFINED-ON-LINE '839) 
(PUT 'LEX_SKIP_LINE_COMMENT 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_SKIP_LINE_COMMENT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LEX_SKIP_LINE_COMMENT NIL
    (PROGN
     (PROG ()
      WHILELABEL
       (COND
        ((NOT (NOT (OR (EQUAL LEX_CHAR $EOL$) (EQUAL LEX_CHAR $EOF$))))
         (RETURN NIL)))
       (YYREADCH)
       (GO WHILELABEL))
     T)) 
(PUT 'LEX_START_BLOCK_COMMENT 'NUMBER-OF-ARGS 1) 
(PUT 'LEX_START_BLOCK_COMMENT 'DEFINED-ON-LINE '843) 
(PUT 'LEX_START_BLOCK_COMMENT 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_START_BLOCK_COMMENT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEX_START_BLOCK_COMMENT (CH)
    (OR
     (AND (EQUAL CH '/) (EQUAL (YYPEEK) '*)
          (NOT (ZEROP (LAND LEXER_STYLE* 8))))
     (AND (EQUAL CH '|(|) (EQUAL (YYPEEK) '*)
          (NOT (ZEROP (LAND LEXER_STYLE* 16)))))) 
(PUT 'LEX_SKIP_BLOCK_COMMENT 'NUMBER-OF-ARGS 0) 
(PUT 'LEX_SKIP_BLOCK_COMMENT 'DEFINED-ON-LINE '847) 
(PUT 'LEX_SKIP_BLOCK_COMMENT 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_SKIP_BLOCK_COMMENT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LEX_SKIP_BLOCK_COMMENT NIL
    (PROG (FLAVOUR TERM)
      (SETQ FLAVOUR LEX_CHAR)
      (COND ((EQUAL FLAVOUR '/) (SETQ TERM '/)) (T (SETQ TERM '|)|)))
      (SETQ LEX_CHAR (YYREADCH))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (OR (NEQ (SETQ LEX_CHAR (YYREADCH)) '*) (NEQ (YYPEEK) TERM))
                (NEQ LEX_CHAR $EOF$)))
          (RETURN NIL)))
        (PROGN
         (COND
          ((AND (NOT (ZEROP (LAND LEXER_STYLE* 32)))
                (LEX_START_BLOCK_COMMENT LEX_CHAR))
           (LEX_SKIP_BLOCK_COMMENT))))
        (GO WHILELABEL))
      (SETQ LEX_CHAR (YYREADCH))
      (RETURN T))) 
(PUT 'LEXER_WORD_STARTER 'NUMBER-OF-ARGS 1) 
(PUT 'LEXER_WORD_STARTER 'DEFINED-ON-LINE '866) 
(PUT 'LEXER_WORD_STARTER 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEXER_WORD_STARTER 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEXER_WORD_STARTER (CH)
    (OR (LITER CH) (AND (EQUAL CH '_) (NOT (ZEROP (LAND LEXER_STYLE* 2048))))
        (AND (EQUAL CH '|'|) (NOT (ZEROP (LAND LEXER_STYLE* 8192))))
        (AND (EQUAL CH '|\\|) (NOT (ZEROP (LAND LEXER_STYLE* 4096)))))) 
(PUT 'LEXER_WORD_CONTINUES 'NUMBER-OF-ARGS 1) 
(PUT 'LEXER_WORD_CONTINUES 'DEFINED-ON-LINE '872) 
(PUT 'LEXER_WORD_CONTINUES 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEXER_WORD_CONTINUES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LEXER_WORD_CONTINUES (CH)
    (OR (LITER CH) (DIGIT CH)
        (AND (EQUAL CH '_) (NOT (ZEROP (LAND LEXER_STYLE* 16384))))
        (AND (EQUAL CH '|.|) (NOT (ZEROP (LAND LEXER_STYLE* 524288))))
        (AND (EQUAL CH '|'|) (NOT (ZEROP (LAND LEXER_STYLE* 65536)))))) 
(PUT 'LEX_BASIC_TOKEN 'NUMBER-OF-ARGS 0) 
(PUT 'LEX_BASIC_TOKEN 'DEFINED-ON-LINE '879) 
(PUT 'LEX_BASIC_TOKEN 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'LEX_BASIC_TOKEN 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LEX_BASIC_TOKEN NIL
    (PROG (R W NEGATE)
      (COND
       (LEX_PEEKED
        (PROGN
         (SETQ R LEX_PEEKED)
         (SETQ YYLVAL LEX_PEEKED_YYLVAL)
         (SETQ LEX_ESCAPED LEX_PEEKED_ESCAPED)
         (SETQ LEX_PEEKED
                 (SETQ LEX_PEEKED_YYLVAL (SETQ LEX_PEEKED_ESCAPED NIL)))
         (RETURN R))))
      (SETQ LEX_ESCAPED NIL)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (OR (EQUAL LEX_CHAR BLANK) (EQUAL LEX_CHAR $EOL$)
               (EQUAL LEX_CHAR '|	|)
               (AND (LEX_START_LINE_COMMENT LEX_CHAR) (LEX_SKIP_LINE_COMMENT))
               (AND (LEX_START_BLOCK_COMMENT LEX_CHAR)
                    (LEX_SKIP_BLOCK_COMMENT))))
          (RETURN NIL)))
        (YYREADCH)
        (GO WHILELABEL))
      (COND
       ((EQUAL LEX_CHAR $EOF$)
        (PROGN (SETQ YYLVAL LEX_CHAR) (RETURN LEX_EOF_CODE))))
      (COND
       ((OR (LEXER_WORD_STARTER LEX_CHAR)
            (AND (EQUAL LEX_CHAR '!) (NOT (ZEROP (LAND LEXER_STYLE* 32768)))
                 (PROGN
                  ((LAMBDA (*RAISE *LOWER) (YYREADCH)) NIL NIL)
                  (SETQ LEX_ESCAPED T))))
        (PROGN
         (SETQ R (CONS LEX_CHAR R))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (OR (LEXER_WORD_CONTINUES (YYREADCH))
                  (AND (EQUAL LEX_CHAR '!)
                       (NOT (ZEROP (LAND LEXER_STYLE* 32768)))
                       (PROGN
                        ((LAMBDA (*RAISE *LOWER) (YYREADCH)) NIL NIL)
                        (SETQ LEX_ESCAPED T)))))
             (RETURN NIL)))
           (SETQ R (CONS LEX_CHAR R))
           (GO WHILELABEL))
         (SETQ YYLVAL (INTERN (LIST2WIDESTRING (SETQ R (REVERSIP R)))))
         (COND
          ((AND (EQUAL YYLVAL 'COMMENT)
                (NOT (ZEROP (LAND LEXER_STYLE* 262144))) (NOT LEX_ESCAPED))
           (PROGN
            (PROG ()
             WHILELABEL
              (COND
               ((NOT (NOT (OR (EQUAL LEX_CHAR '|;|) (EQUAL LEX_CHAR '$))))
                (RETURN NIL)))
              (YYREADCH)
              (GO WHILELABEL))
            (YYREADCH)
            (RETURN (LEX_BASIC_TOKEN)))))
         (COND
          ((AND (NOT LEX_ESCAPED) (SETQ W (GET YYLVAL 'LEX_CODE))) (RETURN W))
          ((OR (EQCAR R '|'|) (GET R 'LEX_IS_TYPENAME))
           (RETURN LEX_TYPENAME_CODE))
          (T (RETURN LEX_SYMBOL_CODE)))))
       ((OR (DIGIT LEX_CHAR)
            (AND (EQUAL LEX_CHAR '~) (NOT (ZEROP (LAND LEXER_STYLE* 131072)))
                 (DIGIT (YYPEEK))))
        (PROGN
         (COND ((EQUAL LEX_CHAR '~) (PROGN (SETQ NEGATE T) (YYREADCH))))
         (COND
          ((AND (EQUAL LEX_CHAR '|0|)
                (OR (EQUAL (YYPEEK) 'X) (EQUAL (YYPEEK) '|x|)))
           (PROGN
            (SETQ YYLVAL 0)
            (YYREADCH)
            (PROG ()
             WHILELABEL
              (COND ((NOT (SETQ W (LEX_HEXVAL (YYREADCH)))) (RETURN NIL)))
              (SETQ YYLVAL (PLUS (TIMES 16 YYLVAL) W))
              (GO WHILELABEL))
            (COND (NEGATE (SETQ YYLVAL (MINUS YYLVAL))))
            (RETURN LEX_NUMBER_CODE))))
         (SETQ R (LIST LEX_CHAR))
         (PROG ()
          WHILELABEL
           (COND ((NOT (PROGN (YYREADCH) (DIGIT LEX_CHAR))) (RETURN NIL)))
           (SETQ R (CONS LEX_CHAR R))
           (GO WHILELABEL))
         (COND
          ((EQUAL LEX_CHAR '|.|)
           (PROGN
            (SETQ W T)
            (SETQ R (CONS LEX_CHAR R))
            (PROG ()
             WHILELABEL
              (COND ((NOT (PROGN (YYREADCH) (DIGIT LEX_CHAR))) (RETURN NIL)))
              (SETQ R (CONS LEX_CHAR R))
              (GO WHILELABEL)))))
         (COND
          ((OR (EQUAL LEX_CHAR 'E) (EQUAL LEX_CHAR '|e|))
           (PROGN
            (COND ((NOT W) (SETQ R (CONS '|0| (CONS '|.| R)))))
            (SETQ W T)
            (SETQ R (CONS 'E R))
            (YYREADCH)
            (COND
             ((OR (EQUAL LEX_CHAR '+) (EQUAL LEX_CHAR '-))
              (PROGN (SETQ R (CONS LEX_CHAR R)) (YYREADCH))))
            (COND ((NOT (DIGIT LEX_CHAR)) (SETQ R (CONS '|0| R)))
                  (T
                   (PROGN
                    (SETQ R (CONS LEX_CHAR R))
                    (PROG ()
                     WHILELABEL
                      (COND
                       ((NOT (PROGN (YYREADCH) (DIGIT LEX_CHAR)))
                        (RETURN NIL)))
                      (SETQ R (CONS LEX_CHAR R))
                      (GO WHILELABEL))))))))
         (SETQ YYLVAL (COMPRESS (REVERSIP R)))
         (COND (NEGATE (SETQ YYLVAL (MINUS YYLVAL))))
         (RETURN LEX_NUMBER_CODE)))
       ((AND (EQUAL LEX_CHAR '|"|) (NOT (ZEROP (LAND LEXER_STYLE* 128))))
        (PROGN
         (COND
          ((NOT (ZEROP (LAND LEXER_STYLE* 1024)))
           (PROG (*RAISE *LOWER)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT
                  (OR (NEQ (YYREADCH) '|"|)
                      (AND (EQUAL (YYPEEK) '|"|) (EQUAL (YYREADCH) '|"|))))
                 (RETURN NIL)))
               (SETQ R (CONS LEX_CHAR R))
               (GO WHILELABEL))))
          (T
           (PROG (*RAISE *LOWER PREV)
             (PROG ()
              WHILELABEL
               (COND
                ((NOT (OR (NEQ (YYREADCH) '|"|) (EQUAL PREV '|\\|)))
                 (RETURN NIL)))
               (PROGN (SETQ PREV LEX_CHAR) (SETQ R (CONS LEX_CHAR R)))
               (GO WHILELABEL)))))
         (YYREADCH)
         (SETQ YYLVAL (LIST2WIDESTRING (REVERSIP R)))
         (RETURN LEX_STRING_CODE)))
       ((AND (EQUAL LEX_CHAR '|'|) (NOT (ZEROP (LAND LEXER_STYLE* 256))))
        (PROGN
         (PROG (*RAISE *LOWER)
           (PROG ()
            REPEATLABEL
             (PROGN
              (PROG ()
               WHILELABEL
                (COND ((NOT (NOT (EQUAL (YYREADCH) '|'|))) (RETURN NIL)))
                (SETQ R (CONS LEX_CHAR R))
                (GO WHILELABEL))
              (SETQ R (CONS LEX_CHAR R))
              (YYREADCH))
             (COND ((NOT (NOT (EQUAL LEX_CHAR '|'|))) (GO REPEATLABEL)))))
         (SETQ YYLVAL (LIST2WIDESTRING (REVERSIP (CDR R))))
         (RETURN LEX_CHAR_CODE)))
       ((AND (EQUAL LEX_CHAR '|'|) (NOT (ZEROP (LAND LEXER_STYLE* 262144))))
        (PROGN
         (YYREADCH)
         (READ_S_EXPRESSION)
         (SETQ YYLVAL (LIST 'QUOTE YYLVAL))
         (RETURN LEX_LIST_CODE)))
       ((AND (EQUAL LEX_CHAR '|`|) (NOT (ZEROP (LAND LEXER_STYLE* 262144))))
        (PROGN
         (YYREADCH)
         (READ_S_EXPRESSION)
         (SETQ YYLVAL (LIST 'BACKQUOTE YYLVAL))
         (RETURN LEX_LIST_CODE)))
       (T
        (PROGN
         (SETQ YYLVAL LEX_CHAR)
         (COND ((EQUAL YYLVAL $EOF$) (RETURN LEX_EOF_CODE)))
         (COND
          ((OR (AND (EQUAL YYLVAL '|#|) (NOT (ZEROP (LAND LEXER_STYLE* 64))))
               (GET YYLVAL 'LEX_DIPTHONG)
               (AND (NOT (ZEROP (LAND LEXER_STYLE* 131072)))
                    (FLAGP YYLVAL 'SML_OPCHAR)))
           (YYREADCH))
          (T (SETQ LEX_CHAR BLANK)))
         (COND
          ((OR
            (AND (EQUAL YYLVAL '|#|) (NOT (ZEROP (LAND LEXER_STYLE* 64)))
                 (LITER LEX_CHAR))
            (EQUAL LEX_CHAR '!))
           (PROGN
            (SETQ R (LEX_BASIC_TOKEN))
            (COND
             ((MEMQ YYLVAL '(IF ELSE ELIF ENDIF DEFINE EVAL))
              (PROGN
               (SETQ YYLVAL
                       (INTERN
                        (LIST2WIDESTRING
                         (CONS '|#|
                               (WIDESTRING2LIST (SYMBOL-NAME YYLVAL))))))))
             (T
              (PROGN
               (SETQ LEX_PEEKED R)
               (SETQ LEX_PEEKED_YYLVAL YYLVAL)
               (SETQ LEX_PEEKED_ESCAPED LEX_ESCAPED)
               (SETQ YYLVAL '|#|)
               (SETQ LEX_ESCAPED NIL)))))))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (SETQ W (ATSOC LEX_CHAR (GET YYLVAL 'LEX_DIPTHONG))))
             (RETURN NIL)))
           (PROGN (SETQ YYLVAL (CDR W)) (YYREADCH))
           (GO WHILELABEL))
         (COND
          ((AND (NOT (ZEROP (LAND LEXER_STYLE* 131072)))
                (ALL_SML_OPCHAR (SETQ W (EXPLODE2 YYLVAL))))
           (PROGN
            (PROG ()
             WHILELABEL
              (COND
               ((NOT
                 (AND (NOT (ZEROP (LAND LEXER_STYLE* 131072)))
                      (FLAGP LEX_CHAR 'SML_OPCHAR)))
                (RETURN NIL)))
              (PROGN (SETQ W (APPEND W (LIST LEX_CHAR))) (YYREADCH))
              (GO WHILELABEL))
            (SETQ YYLVAL (INTERN (LIST2STRING W))))))
         (COND ((SETQ W (GET YYLVAL 'LEX_CODE)) (RETURN W))
               (T (RETURN LEX_SYMBOL_CODE)))))))) 
(SETQ DOT_CHAR (CHAR-CODE '|.|)) 
(SETQ RPAR_CHAR (CHAR-CODE '|)|)) 
(SETQ RSQUARE_CHAR (CHAR-CODE '])) 
(PUT 'READ_S_EXPRESSION 'NUMBER-OF-ARGS 0) 
(PUT 'READ_S_EXPRESSION 'DEFINED-ON-LINE '1130) 
(PUT 'READ_S_EXPRESSION 'DEFINED-IN-FILE 'LALR/YYLEX.RED) 
(PUT 'READ_S_EXPRESSION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_S_EXPRESSION NIL
    (PROGN
     (PROG ()
      WHILELABEL
       (COND
        ((NOT (OR (EQUAL LEX_CHAR BLANK) (EQUAL LEX_CHAR '$EOL$)))
         (RETURN NIL)))
       (YYREADCH)
       (GO WHILELABEL))
     (COND
      ((EQUAL LEX_CHAR '|(|)
       (PROG (R W W1)
         (YYREADCH)
         (SETQ W (READ_S_EXPRESSION))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (NOT (OR (EQUAL W RPAR_CHAR) (EQUAL W DOT_CHAR) (EQUAL W 0))))
             (RETURN NIL)))
           (PROGN (SETQ R (CONS YYLVAL R)) (SETQ W (READ_S_EXPRESSION)))
           (GO WHILELABEL))
         (COND ((NOT (EQUAL W DOT_CHAR)) (SETQ YYLVAL (REVERSIP R)))
               (T
                (PROGN
                 (READ_S_EXPRESSION)
                 (SETQ W YYLVAL)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT R) (RETURN NIL)))
                   (PROGN
                    (SETQ W1 (CDR R))
                    (RPLACD R W)
                    (SETQ W R)
                    (SETQ R W1))
                   (GO WHILELABEL))
                 (SETQ YYLVAL W)
                 (PROG ()
                  WHILELABEL
                   (COND
                    ((NOT (OR (EQUAL LEX_CHAR BLANK) (EQUAL LEX_CHAR '$EOL$)))
                     (RETURN NIL)))
                   (YYREADCH)
                   (GO WHILELABEL))
                 (COND ((EQUAL LEX_CHAR '|)|) (SETQ LEX_CHAR BLANK))
                       (T
                        (YYERROR
                         "Syntax error with '.' notation in a list"))))))
         (RETURN '|:LIST|)))
      ((EQUAL LEX_CHAR '[)
       (PROG (R W W1)
         (YYREADCH)
         (SETQ W (READ_S_EXPRESSION))
         (SETQ W1 (MINUS 1))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT (NOT (OR (EQUAL W RSQUARE_CHAR) (EQUAL W 0)))) (RETURN NIL)))
           (PROGN
            (SETQ R (CONS YYLVAL R))
            (SETQ W1 (PLUS W1 1))
            (SETQ W (READ_S_EXPRESSION)))
           (GO WHILELABEL))
         (SETQ W (MKVECT W1))
         (SETQ R (REVERSIP R))
         (SETQ W1 0)
         (PROG ()
          WHILELABEL
           (COND ((NOT R) (RETURN NIL)))
           (PROGN (PUTV W W1 (CAR R)) (SETQ W1 (PLUS W1 1)) (SETQ R (CDR R)))
           (GO WHILELABEL))
         (SETQ YYLVAL W)
         (RETURN '|:LIST|)))
      ((EQUAL LEX_CHAR '|,|)
       (PROGN
        (YYREADCH)
        (COND
         ((EQUAL LEX_CHAR '@)
          (PROGN
           (YYREADCH)
           (READ_S_EXPRESSION)
           (SETQ YYLVAL (LIST '|,@| YYLVAL))))
         (T (PROGN (READ_S_EXPRESSION) (SETQ YYLVAL (LIST '|,| YYLVAL)))))
        'LIST))
      ((OR (EQUAL LEX_CHAR '|)|) (EQUAL LEX_CHAR ']) (EQUAL LEX_CHAR '|.|))
       (PROGN (SETQ YYLVAL LEX_CHAR) (SETQ LEX_CHAR BLANK) (CHAR-CODE YYLVAL)))
      (T (LEX_BASIC_TOKEN))))) 
(ENDMODULE) 