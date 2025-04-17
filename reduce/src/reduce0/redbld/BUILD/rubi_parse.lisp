(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RUBI_PARSE)) 
(FLUID '(TRAP-TIME*)) 
(SWITCH (LIST 'TRRUBI)) 
(FLUID '(MMACHAR)) 
(SETQ MMACHAR BLANK) 
(GLOBAL '(BLANK TAB)) 
(COND
 ((OR (NOT (BOUNDP 'BLANK)) (NULL BLANK)) (SETQ BLANK (INTERN (INT2ID 32))))) 
(COND ((OR (NOT (BOUNDP 'TAB)) (NULL TAB)) (SETQ TAB (INTERN (INT2ID 9))))) 
(PUT 'FLUSH 'NUMBER-OF-ARGS 1) 
(PUT 'FLUSH 'DEFINED-ON-LINE '52) 
(PUT 'FLUSH 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'FLUSH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FLUSH (U) NIL) 
(FLAG '(FLUSH) 'RLISP) 
(GLOBAL '(CARRIAGE_RETURN)) 
(SETQ CARRIAGE_RETURN (INTERN (INT2ID 13))) 
(PUT 'ISSPACE 'NUMBER-OF-ARGS 1) 
(PUT 'ISSPACE 'DEFINED-ON-LINE '63) 
(PUT 'ISSPACE 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'ISSPACE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ISSPACE (CH)
    (OR (EQUAL CH BLANK) (EQUAL CH TAB) (EQUAL CH $EOL$)
        (EQUAL CH CARRIAGE_RETURN))) 
(FLUID '(CURREXPR PREVLINE CURRLINE EOFCOUNTER LINE_NUMBER)) 
(SETQ PREVLINE (SETQ CURRLINE NIL)) 
(SETQ CURREXPR (LIST $EOL$)) 
(SETQ LINE_NUMBER 1) 
(FLUID '(OUTER_LEVEL)) 
(SETQ OUTER_LEVEL T) 
(PUT 'NEXT_MMA_CHAR 'NUMBER-OF-ARGS 0) 
(PUT 'NEXT_MMA_CHAR 'DEFINED-ON-LINE '81) 
(PUT 'NEXT_MMA_CHAR 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'NEXT_MMA_CHAR 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEXT_MMA_CHAR NIL
    (PROG (R)
      (SETQ R (READCH))
      (COND
       ((GREATERP EOFCOUNTER 0)
        (PROGN
         (COND ((GREATERP EOFCOUNTER 5) (RETURN $EOF$)))
         (SETQ EOFCOUNTER (PLUS EOFCOUNTER 1))
         (RETURN $EOL$)))
       ((EQUAL R $EOF$)
        (PROGN
         (SETQ CURRLINE (CONS 'EOF CURRLINE))
         (SETQ EOFCOUNTER 1)
         (RETURN $EOL$)))
       ((EQUAL R $EOL$)
        (PROGN
         (SETQ CURREXPR (CONS R CURREXPR))
         (SETQ LINE_NUMBER (PLUS LINE_NUMBER 1))
         (SETQ PREVLINE CURRLINE)
         (SETQ CURRLINE NIL)))
       (T
        (PROGN
         (SETQ CURREXPR (CONS R CURREXPR))
         (SETQ CURRLINE (CONS R CURRLINE)))))
      (RETURN R))) 
(FLUID '(MMATOK *TRACE_TOKEN)) 
(SETQ *TRACE_TOKEN NIL) 
(PUT 'NEXT_MMA_TOK 'NUMBER-OF-ARGS 0) 
(PUT 'NEXT_MMA_TOK 'DEFINED-ON-LINE '112) 
(PUT 'NEXT_MMA_TOK 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'NEXT_MMA_TOK 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEXT_MMA_TOK NIL
    (PROG (R)
      (SETQ R (NEXT_MMA_TOK1))
      (COND
       (*TRACE_TOKEN
        (PROGN
         (PRINC "Token: ")
         (COND ((EQUAL R $EOF$) (PROGN (PRIN2 "EOF") (TERPRI) "EOF"))
               ((EQUAL R $EOL$) (PROGN (PRIN2 "EOL") (TERPRI) "EOL"))
               ((EQUAL R BLANK) (PROGN (PRIN2 "BLANK") (TERPRI) "BLANK"))
               ((EQUAL R TAB) (PROGN (PRIN2 "TAB") (TERPRI) "TAB"))
               ((AND (IDP R) (FLAGP R 'OPERATOR_LIKE))
                (PROGN
                 (PRINC "operator \"")
                 (PRINC R)
                 (PROGN (PRIN2 "\"") (TERPRI) "\"")))
               (T (PRINT R))))))
      (RETURN R))) 
(PUT 'NEXT_MMA_TOK1 'NUMBER-OF-ARGS 0) 
(PUT 'NEXT_MMA_TOK1 'DEFINED-ON-LINE '130) 
(PUT 'NEXT_MMA_TOK1 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'NEXT_MMA_TOK1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE NEXT_MMA_TOK1 NIL
    (PROG (W BUFF SAVE*RAISE SAVE*LOWER)
      (COND ((EQUAL MMACHAR $EOF$) (RETURN (SETQ MMATOK MMACHAR))))
      (SETQ SAVE*RAISE *RAISE)
      (SETQ SAVE*LOWER *LOWER)
      (SETQ *RAISE (SETQ *LOWER NIL))
      (SETQ W 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT (ISSPACE MMACHAR)) (RETURN NIL)))
        (PROGN
         (COND ((EQUAL MMACHAR $EOL$) (SETQ W (PLUS W 1))))
         (SETQ MMACHAR (NEXT_MMA_CHAR)))
        (GO WHILELABEL))
      (COND
       ((AND OUTER_LEVEL
             (OR (AND (GREATERP W 0) (EQUAL MMACHAR $EOF$)) (GREATERP W 1)))
        (PROGN
         (SETQ *RAISE SAVE*RAISE)
         (SETQ *LOWER SAVE*LOWER)
         (RETURN (SETQ MMATOK $EOL$)))))
      (COND
       ((DIGIT MMACHAR)
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT (DIGIT MMACHAR)) (RETURN NIL)))
           (PROGN
            (SETQ BUFF (CONS MMACHAR BUFF))
            (SETQ MMACHAR (NEXT_MMA_CHAR)))
           (GO WHILELABEL))
         (COND
          ((EQUAL MMACHAR '|.|)
           (PROGN
            (SETQ BUFF (CONS MMACHAR BUFF))
            (SETQ MMACHAR (NEXT_MMA_CHAR))
            (PROG ()
             WHILELABEL
              (COND ((NOT (DIGIT MMACHAR)) (RETURN NIL)))
              (PROGN
               (SETQ BUFF (CONS MMACHAR BUFF))
               (SETQ MMACHAR (NEXT_MMA_CHAR)))
              (GO WHILELABEL)))))
         (SETQ *RAISE SAVE*RAISE)
         (SETQ *LOWER SAVE*LOWER)
         (RETURN (SETQ MMATOK (COMPRESS (REVERSE BUFF))))))
       ((OR (LITER MMACHAR) (EQUAL MMACHAR '$))
        (PROGN
         (COND
          ((EQUAL MMACHAR '$)
           (PROGN (SETQ BUFF '($ !)) (SETQ MMACHAR (NEXT_MMA_CHAR)))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (OR (LITER MMACHAR) (DIGIT MMACHAR))) (RETURN NIL)))
           (PROGN
            (SETQ BUFF (CONS MMACHAR BUFF))
            (SETQ MMACHAR (NEXT_MMA_CHAR)))
           (GO WHILELABEL))
         (SETQ MMATOK (INTERN (COMPRESS (REVERSE BUFF))))
         (SETQ *RAISE SAVE*RAISE)
         (SETQ *LOWER SAVE*LOWER)
         (RETURN MMATOK)))
       ((EQUAL MMACHAR '|"|)
        (PROGN
         (SETQ BUFF NIL)
         (SETQ MMACHAR (NEXT_MMA_CHAR))
         (PROG ()
          WHILELABEL
           (COND ((NOT (NOT (EQUAL MMACHAR '|"|))) (RETURN NIL)))
           (PROGN
            (SETQ BUFF (CONS MMACHAR BUFF))
            (COND ((EQUAL MMACHAR $EOF$) (SYNTAX_ERROR "unterminated string")))
            (SETQ MMACHAR (NEXT_MMA_CHAR)))
           (GO WHILELABEL))
         (SETQ MMACHAR (NEXT_MMA_CHAR))
         (SETQ BUFF (REVERSE BUFF))
         (SETQ MMATOK (LIST2STRING BUFF))
         (SETQ *RAISE SAVE*RAISE)
         (SETQ *LOWER SAVE*LOWER)
         (RETURN MMATOK)))
       ((EQUAL MMACHAR '|#|)
        (PROGN
         (SETQ BUFF (LIST MMACHAR '!))
         (SETQ MMACHAR (NEXT_MMA_CHAR))
         (COND
          ((EQUAL MMACHAR '|#|)
           (PROGN
            (SETQ BUFF (CONS MMACHAR (CONS '! BUFF)))
            (SETQ MMACHAR (NEXT_MMA_CHAR)))))
         (PROG ()
          WHILELABEL
           (COND ((NOT (DIGIT MMACHAR)) (RETURN NIL)))
           (PROGN
            (SETQ BUFF (CONS MMACHAR BUFF))
            (SETQ MMACHAR (NEXT_MMA_CHAR)))
           (GO WHILELABEL))
         (SETQ MMATOK (INTERN (COMPRESS (REVERSE BUFF))))
         (SETQ *RAISE SAVE*RAISE)
         (SETQ *LOWER SAVE*LOWER)
         (RETURN MMATOK))))
      (SETQ BUFF MMACHAR)
      (SETQ MMACHAR (NEXT_MMA_CHAR))
      (COND
       ((NULL (MEMQ MMACHAR (GET BUFF 'FOLLOW_ON_CHAR)))
        (PROGN
         (FLAG (LIST BUFF) 'OPERATOR_LIKE)
         (SETQ *RAISE SAVE*RAISE)
         (SETQ *LOWER SAVE*LOWER)
         (RETURN (SETQ MMATOK BUFF)))))
      (SETQ BUFF (INTERN (COMPRESS (LIST '! BUFF '! MMACHAR))))
      (SETQ MMACHAR (NEXT_MMA_CHAR))
      (COND
       ((AND (OR (EQUAL BUFF '==) (EQUAL BUFF '=!)) (EQUAL MMACHAR '=))
        (PROGN
         (SETQ MMACHAR (NEXT_MMA_CHAR))
         (COND ((EQUAL BUFF '==) (SETQ BUFF '===)) (T (SETQ BUFF '=!=))))))
      (COND
       ((EQUAL BUFF '|(*|)
        (PROGN
         (NEXT_MMA_TOK1)
         (PROG ()
          WHILELABEL
           (COND ((NOT (NEQ MMATOK '|*)|)) (RETURN NIL)))
           (PROGN
            (COND
             ((EQUAL MMATOK $EOF$)
              (SYNTAX_ERROR "end of file within comment")))
            (NEXT_MMA_TOK1))
           (GO WHILELABEL))
         (SETQ *RAISE SAVE*RAISE)
         (SETQ *LOWER SAVE*LOWER)
         (RETURN (NEXT_MMA_TOK1)))))
      (FLAG (LIST BUFF) 'OPERATOR_LIKE)
      (SETQ *RAISE SAVE*RAISE)
      (SETQ *LOWER SAVE*LOWER)
      (RETURN (SETQ MMATOK BUFF)))) 
(FLAG (LIST $EOL$ $EOF$) 'OPERATOR_LIKE) 
(PUT '|:| 'FOLLOW_ON_CHAR '(=)) 
(PUT '/ 'FOLLOW_ON_CHAR '(|;|)) 
(PUT '|(| 'FOLLOW_ON_CHAR '(*)) 
(PUT '* 'FOLLOW_ON_CHAR '(|)|)) 
(PUT '& 'FOLLOW_ON_CHAR '(&)) 
(PUT '|\|| 'FOLLOW_ON_CHAR '(|\||)) 
(PUT '> 'FOLLOW_ON_CHAR '(=)) 
(PUT '< 'FOLLOW_ON_CHAR '(=)) 
(PUT '! 'FOLLOW_ON_CHAR '(=)) 
(PUT '= 'FOLLOW_ON_CHAR '(= !)) 
(PUT '_ 'FOLLOW_ON_CHAR '(|.| |'|)) 
(SETQ MMATOK NIL) 
(PUT 'PARSERULES 'NUMBER-OF-ARGS 0) 
(PUT 'PARSERULES 'DEFINED-ON-LINE '329) 
(PUT 'PARSERULES 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSERULES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSERULES NIL
    (PROG (R)
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL MMATOK $EOL$)) (RETURN NIL)))
        (NEXT_MMA_TOK)
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (EQUAL MMATOK $EOF$))) (RETURN NIL)))
        (PROGN
         (SETQ R (CONS (PARSERULE) R))
         (PROG ()
          WHILELABEL
           (COND ((NOT (EQUAL MMATOK $EOL$)) (RETURN NIL)))
           (NEXT_MMA_TOK)
           (GO WHILELABEL)))
        (GO WHILELABEL))
      (RETURN (REVERSE R)))) 
(PUT 'PARSERULE 'NUMBER-OF-ARGS 0) 
(PUT 'PARSERULE 'DEFINED-ON-LINE '342) 
(PUT 'PARSERULE 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSERULE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSERULE NIL (PARSEEXPRESSION)) 
(PUT 'PARSEEXPRESSION 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEEXPRESSION 'DEFINED-ON-LINE '348) 
(PUT 'PARSEEXPRESSION 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEEXPRESSION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEEXPRESSION NIL
    (PROG (R)
      (SETQ R (PARSEE0))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL MMATOK '|;|)) (RETURN NIL)))
        (PROGN (NEXT_MMA_TOK) (SETQ R (LIST '|;| R (PARSEE0))))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'PARSEE0 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE0 'DEFINED-ON-LINE '361) 
(PUT 'PARSEE0 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE0 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE0 NIL
    (PROG (R)
      (SETQ R (PARSEE1))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL MMATOK '|/;|)) (RETURN NIL)))
        (PROGN (NEXT_MMA_TOK) (SETQ R (LIST '|/;| R (PARSEE1))))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'PARSEE1 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE1 'DEFINED-ON-LINE '373) 
(PUT 'PARSEE1 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE1 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE1 NIL
    (PROG (R)
      (SETQ R (PARSEE2))
      (COND
       ((EQUAL MMATOK '|:=|)
        (PROGN (NEXT_MMA_TOK) (SETQ R (LIST '|:=| R (PARSEE2))))))
      (RETURN R))) 
(PUT 'PARSEE2 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE2 'DEFINED-ON-LINE '387) 
(PUT 'PARSEE2 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE2 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE2 NIL
    (PROG (R)
      (SETQ R (PARSEE3))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL MMATOK '|\|\||)) (RETURN NIL)))
        (PROGN (NEXT_MMA_TOK) (SETQ R (LIST 'OR R (PARSEE3))))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'PARSEE3 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE3 'DEFINED-ON-LINE '400) 
(PUT 'PARSEE3 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE3 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE3 NIL
    (PROG (R)
      (SETQ R (PARSEE4))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL MMATOK '&&)) (RETURN NIL)))
        (PROGN (NEXT_MMA_TOK) (SETQ R (LIST 'AND R (PARSEE4))))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'PARSEE4CHAIN 'NUMBER-OF-ARGS 4) 
(PUT 'PARSEE4CHAIN 'DEFINED-ON-LINE '436) 
(PUT 'PARSEE4CHAIN 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE4CHAIN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PARSEE4CHAIN (R A TOK ALLOWED)
    (PROG (B)
      (NEXT_MMA_TOK)
      (SETQ B (PARSEE5))
      (SETQ R (CONS (LIST (GET TOK 'REDUCE_FORM) A B) R))
      (COND
       ((MEMBER MMATOK ALLOWED) (RETURN (PARSEE4CHAIN R B MMATOK ALLOWED))))
      (COND ((NULL (CDR R)) (RETURN (CAR R)))
            (T (RETURN (CONS 'AND (REVERSE R))))))) 
(PUT 'PARSEE4 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE4 'DEFINED-ON-LINE '448) 
(PUT 'PARSEE4 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE4 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE4 NIL
    (PROG (A)
      (SETQ A (PARSEE5))
      (COND
       ((OR (EQUAL MMATOK '<) (EQUAL MMATOK '<=))
        (RETURN (PARSEE4CHAIN NIL A MMATOK '(< <=))))
       ((OR (EQUAL MMATOK '>) (EQUAL MMATOK '>=))
        (RETURN (PARSEE4CHAIN NIL A MMATOK '(> >=))))
       ((OR (EQUAL MMATOK '==) (EQUAL MMATOK '!=))
        (RETURN (PARSEE4CHAIN NIL A MMATOK '(== !=))))
       ((OR (EQUAL MMATOK '===) (EQUAL MMATOK '=!=))
        (RETURN (PARSEE4CHAIN NIL A MMATOK '(=== =!=))))
       (T (RETURN A))))) 
(PUT '> 'REDUCE_FORM 'GREATERP) 
(PUT '< 'REDUCE_FORM 'LESSP) 
(PUT '>= 'REDUCE_FORM 'GEQ) 
(PUT '<= 'REDUCE_FORM 'LEQ) 
(PUT '== 'REDUCE_FORM 'EQUAL) 
(PUT '!= 'REDUCE_FORM 'NEQ) 
(PUT 'PARSEE5 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE5 'DEFINED-ON-LINE '474) 
(PUT 'PARSEE5 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE5 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE5 NIL
    (PROG (R OP)
      (SETQ R (PARSEE6))
      (PROG ()
       WHILELABEL
        (COND ((NOT (OR (EQUAL MMATOK '+) (EQUAL MMATOK '-))) (RETURN NIL)))
        (PROGN
         (SETQ OP MMATOK)
         (NEXT_MMA_TOK)
         (SETQ R (LIST (GET OP 'REDUCE_FORM) R (PARSEE6))))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT '+ 'REDUCE_FORM 'PLUS) 
(PUT '- 'REDUCE_FORM 'DIFFERENCE) 
(PUT 'PARSEE6 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE6 'DEFINED-ON-LINE '493) 
(PUT 'PARSEE6 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE6 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE6 NIL
    (PROG (R OP)
      (SETQ R (PARSEE7))
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (OR (EQUAL MMATOK '*) (EQUAL MMATOK '/)
               (AND (IDP MMATOK) (NOT (FLAGP MMATOK 'OPERATOR_LIKE))
                    (NEQ MMATOK $EOL$) (NEQ MMATOK $EOF$))
               (NUMBERP MMATOK) (EQUAL MMATOK '|(|)))
          (RETURN NIL)))
        (PROGN
         (COND
          ((OR (EQUAL MMATOK '*) (EQUAL MMATOK '/))
           (PROGN (SETQ OP MMATOK) (NEXT_MMA_TOK)))
          (T (SETQ OP '*)))
         (SETQ R (LIST (GET OP 'REDUCE_FORM) R (PARSEE7))))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT '* 'REDUCE_FORM 'TIMES) 
(PUT '/ 'REDUCE_FORM 'QUOTIENT) 
(PUT 'PARSEE7 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE7 'DEFINED-ON-LINE '515) 
(PUT 'PARSEE7 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE7 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE7 NIL
    (PROG (R)
      (COND
       ((EQUAL MMATOK '-)
        (PROGN (NEXT_MMA_TOK) (SETQ R (LIST 'MINUS (PARSEE7)))))
       (T (SETQ R (PARSEE8))))
      (COND
       ((EQUAL MMATOK '^)
        (PROGN (NEXT_MMA_TOK) (RETURN (LIST 'EXPT R (PARSEE7)))))
       (T (RETURN R))))) 
(PUT 'PARSEE8 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE8 'DEFINED-ON-LINE '532) 
(PUT 'PARSEE8 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE8 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE8 NIL
    (PROG (R)
      (SETQ R (PARSEE9))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQUAL MMATOK '!)) (RETURN NIL)))
        (PROGN (NEXT_MMA_TOK) (SETQ R (LIST 'FACTORIAL R)))
        (GO WHILELABEL))
      (RETURN R))) 
(PUT 'PARSEE9 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEE9 'DEFINED-ON-LINE '563) 
(PUT 'PARSEE9 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEE9 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEE9 NIL
    (PROG (R W)
      (COND ((EQUAL MMATOK $EOF$) (RETURN MMATOK))
            ((EQUAL MMATOK '|(|)
             (PROGN
              ((LAMBDA (OUTER_LEVEL)
                 (PROGN (NEXT_MMA_TOK) (SETQ R (PARSEEXPRESSION))))
               NIL)
              (CHECKFOR '|)|)
              (RETURN R)))
            ((EQUAL MMATOK '{)
             (PROGN
              ((LAMBDA (OUTER_LEVEL)
                 (PROGN
                  (NEXT_MMA_TOK)
                  (SETQ R (PARSEEXPRESSION))
                  (SETQ R (CONS 'BRACELIST (CONS R (PARSEBRACETAIL))))))
               NIL)
              (NEXT_MMA_TOK)
              (RETURN R)))
            ((EQUAL MMATOK '[)
             (PROGN
              ((LAMBDA (OUTER_LEVEL)
                 (PROGN
                  (NEXT_MMA_TOK)
                  (SETQ R (PARSEEXPRESSION))
                  (SETQ R (CONS 'BRACKETLIST (CONS R (PARSEARGTAIL))))))
               NIL)
              (NEXT_MMA_TOK)
              (RETURN R)))
            ((OR (NUMBERP MMATOK) (STRINGP MMATOK))
             (PROGN (SETQ R MMATOK) (NEXT_MMA_TOK) (RETURN R)))
            ((EQUAL MMATOK $EOF$) (SYNTAX_ERROR "unexpected end of file"))
            ((FLAGP MMATOK 'OPERATOR_LIKE)
             (SYNTAX_ERROR (LIST "unexpected operator" MMATOK))))
      (SETQ R MMATOK)
      (NEXT_MMA_TOK)
      (COND
       ((EQUAL MMATOK '=)
        (PROGN (NEXT_MMA_TOK) (RETURN (LIST 'SETQ R (PARSEEXPRESSION)))))
       ((EQUAL MMATOK '[)
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT (EQUAL MMATOK '[)) (RETURN NIL)))
           (PROGN
            ((LAMBDA (OUTER_LEVEL)
               (PROGN
                (NEXT_MMA_TOK)
                (SETQ W (PARSEEXPRESSION))
                (SETQ R (CONS R (CONS W (PARSEARGTAIL))))))
             NIL)
            (NEXT_MMA_TOK))
           (GO WHILELABEL))
         (RETURN R)))
       ((EQUAL MMATOK '_)
        (PROGN
         (SETQ R (LIST '_ R))
         (NEXT_MMA_TOK)
         (COND
          ((EQUAL MMATOK '[)
           (PROGN
            ((LAMBDA (OUTER_LEVEL)
               (PROGN
                (NEXT_MMA_TOK)
                (SETQ W (PARSEEXPRESSION))
                (SETQ R (CONS R (CONS W (PARSEARGTAIL))))))
             NIL)
            (NEXT_MMA_TOK)
            (RETURN R)))
          ((AND (IDP MMATOK) (NOT (FLAGP MMATOK 'OPERATOR_LIKE)))
           (PROGN
            (SETQ R (LIST '_ (CADR R) MMATOK))
            (NEXT_MMA_TOK)
            (RETURN R)))
          (T (RETURN R)))))
       ((EQUAL MMATOK '_.) (PROGN (NEXT_MMA_TOK) (RETURN (LIST '_. R))))
       ((EQUAL MMATOK '|_'|)
        (PROGN
         (SETQ R (LIST 'DERIV (LIST '_ R)))
         (NEXT_MMA_TOK)
         (COND
          ((EQUAL MMATOK '[)
           (PROGN
            (PROG ()
             WHILELABEL
              (COND ((NOT (EQUAL MMATOK '[)) (RETURN NIL)))
              (PROGN
               ((LAMBDA (OUTER_LEVEL)
                  (PROGN
                   (NEXT_MMA_TOK)
                   (SETQ W (PARSEEXPRESSION))
                   (SETQ R (CONS R (CONS W (PARSEARGTAIL))))))
                NIL)
               (NEXT_MMA_TOK))
              (GO WHILELABEL))
            (RETURN R)))
          (T (RETURN R)))))
       (T (RETURN R))))) 
(PUT 'PARSEARGTAIL 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEARGTAIL 'DEFINED-ON-LINE '642) 
(PUT 'PARSEARGTAIL 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEARGTAIL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEARGTAIL NIL
    (PROG (R)
      (COND ((EQUAL MMATOK ']) (RETURN NIL)))
      (CHECKFOR '|,|)
      (SETQ R (PARSEEXPRESSION))
      (RETURN (CONS R (PARSEARGTAIL))))) 
(PUT 'PARSEBRACETAIL 'NUMBER-OF-ARGS 0) 
(PUT 'PARSEBRACETAIL 'DEFINED-ON-LINE '651) 
(PUT 'PARSEBRACETAIL 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PARSEBRACETAIL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE PARSEBRACETAIL NIL
    (PROG (R)
      (COND ((EQUAL MMATOK '}) (RETURN NIL)))
      (CHECKFOR '|,|)
      (SETQ R (PARSEEXPRESSION))
      (RETURN (CONS R (PARSEBRACETAIL))))) 
(PUT 'CHECKFOR 'NUMBER-OF-ARGS 1) 
(PUT 'CHECKFOR 'DEFINED-ON-LINE '661) 
(PUT 'CHECKFOR 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'CHECKFOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECKFOR (X)
    (COND ((EQUAL MMATOK X) (NEXT_MMA_TOK))
          (T (SYNTAX_ERROR (LIST X "expected"))))) 
(FLUID '(FILENAME)) 
(PUT 'SYNTAX_ERROR 'NUMBER-OF-ARGS 1) 
(PUT 'SYNTAX_ERROR 'DEFINED-ON-LINE '671) 
(PUT 'SYNTAX_ERROR 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'SYNTAX_ERROR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SYNTAX_ERROR (MESSAGE)
    (PROG ()
      (TERPRI)
      (PRINC "+++ Syntax error in rules file ")
      (PRINT FILENAME)
      (COND ((ATOM MESSAGE) (PRINC MESSAGE))
            (T
             (PROG (X)
               (SETQ X MESSAGE)
              LAB
               (COND ((NULL X) (RETURN NIL)))
               ((LAMBDA (X) (PROGN (PRINC X) (PRINC " "))) (CAR X))
               (SETQ X (CDR X))
               (GO LAB))))
      (TERPRI)
      (PRINC "Line ")
      (PRINC LINE_NUMBER)
      (PROGN (PRIN2 "...") (TERPRI) "...")
      (PROG (X)
        (SETQ X (REVERSE PREVLINE))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (PRINC X)) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (TERPRI)
      (PROG (X)
        (SETQ X (REVERSE CURRLINE))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (PRINC X)) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (TERPRI)
      (TERPRI)
      (ERROR 0 "parsing failed"))) 
(PUT 'FINDVARS 'NUMBER-OF-ARGS 2) 
(PUT 'FINDVARS 'DEFINED-ON-LINE '705) 
(PUT 'FINDVARS 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'FINDVARS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDVARS (U L)
    (COND
     ((OR (ATOM U) (GET (CAR U) 'DNAME) (EQCAR U '*SQ))
      (PROGN
       (COND ((MEMQ U '(E I PI)) L) ((NOT (IDP U)) L) ((MEMQ U L) L)
             (T (CONS U L)))))
     (T (FINDVARSLIST (CDR U) L)))) 
(PUT 'FINDVARSLIST 'NUMBER-OF-ARGS 2) 
(PUT 'FINDVARSLIST 'DEFINED-ON-LINE '713) 
(PUT 'FINDVARSLIST 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'FINDVARSLIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FINDVARSLIST (U L)
    (COND ((ATOM U) L) (T (FINDVARSLIST (CDR U) (FINDVARS (CAR U) L))))) 
(PUT 'PROBABLY_ZERO 'NUMBER-OF-ARGS 2) 
(PUT 'PROBABLY_ZERO 'DEFINED-ON-LINE '727) 
(PUT 'PROBABLY_ZERO 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PROBABLY_ZERO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROBABLY_ZERO (U V)
    (PROG (R)
      (SETQ R
              (WITH-TIMEOUT 10000
                            (LIST 'ERRORSET
                                  (MKQUOTE
                                   (LIST 'PROBABLY_ZERO1 (MKQUOTE U)
                                         (MKQUOTE V)))
                                  T T)))
      (COND
       ((ATOM R)
        (PROGN
         (PROGN
          (PRIN2 "Resource limit exceeded checking equality")
          (TERPRI)
          "Resource limit exceeded checking equality")
         (RETURN NIL))))
      (SETQ R (CAR R))
      (COND
       ((ATOM R)
        (PROGN
         (PROGN
          (PRIN2 "Error while checking equality")
          (TERPRI)
          "Error while checking equality")
         (RETURN NIL)))
       (T (RETURN (CAR R)))))) 
(PUT 'PROBABLY_ZERO1 'NUMBER-OF-ARGS 2) 
(PUT 'PROBABLY_ZERO1 'DEFINED-ON-LINE '743) 
(PUT 'PROBABLY_ZERO1 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'PROBABLY_ZERO1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PROBABLY_ZERO1 (U V)
    (PROG (VARS SUBS U1 V1 SCALE TRIALS *MSG)
      (SETQ VARS (FINDVARS V (FINDVARS U NIL)))
      (SETQ U1 0.0)
      (SETQ SCALE 0.002)
      (SETQ TRIALS 0)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND (OR (LESSP U1 1.0e-5) (GREATERP U1 100000.0))
                (LESSP TRIALS 100)))
          (RETURN NIL)))
        (PROGN
         (SETQ TRIALS (PLUS TRIALS 1))
         (SETQ SUBS
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V VARS)
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (V)
                                       (CONS V
                                             (QUOTIENT
                                              (DIFFERENCE
                                               (RANDOM (TIMES SCALE 2000000))
                                               (TIMES SCALE 1000000.0))
                                              1000003.0)))
                                     (CAR V))
                                    NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (V)
                               (CONS V
                                     (QUOTIENT
                                      (DIFFERENCE
                                       (RANDOM (TIMES SCALE 2000000))
                                       (TIMES SCALE 1000000.0))
                                      1000003.0)))
                             (CAR V))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (ON (LIST 'ROUNDED 'COMPLEX 'NUMVAL))
         (SETQ U1 (PREPSQ (SIMP* (SUBLA SUBS (LIST 'ABS U)))))
         (OFF (LIST 'NUMVAL 'COMPLEX 'ROUNDED))
         (SETQ SCALE (TIMES 2.0 SCALE))
         (COND ((EQCAR U1 '|:RD:|) (SETQ U1 (CDR U1))))
         (COND ((NOT (FLOATP U1)) (SETQ U1 0.0))))
        (GO WHILELABEL))
      (COND ((GEQ TRIALS 100) (RETURN NIL)))
      (ON (LIST 'ROUNDED 'COMPLEX 'NUMVAL))
      (SETQ V1 (PREPSQ (SIMP* (SUBLA SUBS V))))
      (OFF (LIST 'NUMVAL 'COMPLEX 'ROUNDED))
      (COND ((EQCAR V1 '|:RD:|) (SETQ V1 (CDR V1)))
            ((FIXP V1) (SETQ V1 (FLOAT V1))))
      (COND ((NOT (FLOATP V1)) (SETQ V1 (TIMES 10000.0 U1))))
      (RETURN (LESSP (TIMES 10000.0 V1) U1)))) 
(RANDOM_NEW_SEED
 (PROG (W)
   (SETQ W 1)
   (PROG (C)
     (SETQ C (EXPLODE (DATE-AND-TIME)))
    LAB
     (COND ((NULL C) (RETURN NIL)))
     ((LAMBDA (C)
        (SETQ W (REMAINDER (PLUS (TIMES 169 W) (ID2INT C)) 100000000)))
      (CAR C))
     (SETQ C (CDR C))
     (GO LAB))
   (RETURN W))) 
(FLUID '(TESTDIRECTORY)) 
(FLUID '(TESTCOUNT OPTIMAL MESSY UNABLE INVALID)) 
(FLUID '(T_TESTCOUNT T_OPTIMAL T_MESSY T_UNABLE T_INVALID)) 
(FLUID '(SECTION_NAME)) 
(SETQ TESTCOUNT (SETQ OPTIMAL (SETQ MESSY (SETQ UNABLE (SETQ INVALID 0))))) 
(SETQ T_TESTCOUNT
        (SETQ T_OPTIMAL (SETQ T_MESSY (SETQ T_UNABLE (SETQ T_INVALID 0))))) 
(PUT 'START_WHOLE_TEST 'NUMBER-OF-ARGS 0) 
(PUT 'START_WHOLE_TEST 'DEFINED-ON-LINE '807) 
(PUT 'START_WHOLE_TEST 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'START_WHOLE_TEST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE START_WHOLE_TEST NIL
    (PROG ()
      (SETQ T_TESTCOUNT
              (SETQ T_OPTIMAL
                      (SETQ T_MESSY (SETQ T_UNABLE (SETQ T_INVALID 0)))))
      (TERPRI)
      (PRINC "@Integrand types")
      (TTAB 30)
      (PRINC "Problems")
      (TTAB 40)
      (PRINC "Optimal")
      (TTAB 50)
      (PRINC "Messy")
      (TTAB 60)
      (PRINC "Unable")
      (TTAB 70)
      (PROGN (PRIN2 "Invalid") (TERPRI) "Invalid"))) 
(PUT 'START_TEST_SECTION 'NUMBER-OF-ARGS 1) 
(PUT 'START_TEST_SECTION 'DEFINED-ON-LINE '824) 
(PUT 'START_TEST_SECTION 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'START_TEST_SECTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE START_TEST_SECTION (NAME)
    (PROG ()
      (SETQ SECTION_NAME NAME)
      (SETQ TESTCOUNT
              (SETQ OPTIMAL (SETQ MESSY (SETQ UNABLE (SETQ INVALID 0))))))) 
(PUT 'REPORT_SECTION 'NUMBER-OF-ARGS 0) 
(PUT 'REPORT_SECTION 'DEFINED-ON-LINE '831) 
(PUT 'REPORT_SECTION 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'REPORT_SECTION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REPORT_SECTION NIL
    (PROG ()
      (PRINC "@")
      (PRINC SECTION_NAME)
      (TTAB 30)
      (PRIN1 (PLUS OPTIMAL MESSY UNABLE INVALID))
      (TTAB 40)
      (PRIN1 OPTIMAL)
      (TTAB 50)
      (PRIN1 MESSY)
      (TTAB 60)
      (PRIN1 UNABLE)
      (TTAB 70)
      (PRINT INVALID)
      (SETQ T_OPTIMAL (PLUS T_OPTIMAL OPTIMAL))
      (SETQ T_MESSY (PLUS T_MESSY MESSY))
      (SETQ T_UNABLE (PLUS T_UNABLE UNABLE))
      (SETQ T_INVALID (PLUS T_INVALID INVALID)))) 
(PUT 'FINAL_REPORT 'NUMBER-OF-ARGS 0) 
(PUT 'FINAL_REPORT 'DEFINED-ON-LINE '851) 
(PUT 'FINAL_REPORT 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'FINAL_REPORT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FINAL_REPORT NIL
    (PROG (TOT)
      (SETQ TOT (PLUS T_OPTIMAL T_MESSY T_UNABLE T_INVALID))
      (PRINC "@Totals")
      (TTAB 30)
      (PRIN1 TOT)
      (TTAB 40)
      (PRIN1 T_OPTIMAL)
      (TTAB 50)
      (PRIN1 T_MESSY)
      (TTAB 60)
      (PRIN1 T_UNABLE)
      (TTAB 70)
      (PRINT T_INVALID)
      (SETQ TOT (FLOAT TOT))
      (SETPRINTPRECISION 3)
      (PRINC "@Percentages")
      (TTAB 40)
      (PRIN1 (TIMES 100.0 (QUOTIENT (FLOAT T_OPTIMAL) TOT)))
      (TTAB 50)
      (PRIN1 (TIMES 100.0 (QUOTIENT (FLOAT T_MESSY) TOT)))
      (TTAB 60)
      (PRIN1 (TIMES 100.0 (QUOTIENT (FLOAT T_UNABLE) TOT)))
      (TTAB 70)
      (PRINT (TIMES 100.0 (QUOTIENT (FLOAT T_INVALID) TOT))))) 
(PUT 'XSIZE 'NUMBER-OF-ARGS 1) 
(PUT 'XSIZE 'DEFINED-ON-LINE '879) 
(PUT 'XSIZE 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'XSIZE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XSIZE (U)
    (COND ((NULL U) 0) ((ATOM U) 1)
          (T (PLUS 1 (XSIZE (CAR U)) (XSIZE (CDR U)))))) 
(PUT 'CONVERT_TO_REDUCE 'NUMBER-OF-ARGS 1) 
(PUT 'CONVERT_TO_REDUCE 'DEFINED-ON-LINE '884) 
(PUT 'CONVERT_TO_REDUCE 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'CONVERT_TO_REDUCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CONVERT_TO_REDUCE (U)
    (COND ((AND (IDP U) (GET U 'REDUCEVERSION)) (GET U 'REDUCEVERSION))
          ((ATOM U) U)
          (T
           (PROG (V FORALL-RESULT FORALL-ENDPTR)
             (SETQ V U)
             (COND ((NULL V) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS ((LAMBDA (V) (CONVERT_TO_REDUCE V)) (CAR V))
                                   NIL)))
            LOOPLABEL
             (SETQ V (CDR V))
             (COND ((NULL V) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (V) (CONVERT_TO_REDUCE V)) (CAR V)) NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT '|e| 'REDUCEVERSION 'E) 
(PUT '|pI| 'REDUCEVERSION 'PI) 
(PUT '|i| 'REDUCEVERSION 'I) 
(PUT 'E 'REDUCEVERSION 'LOWER_CASE_E) 
(PUT 'I 'REDUCEVERSION 'LOWER_CASE_I) 
(PUT '|iNT| 'REDUCEVERSION 'INT) 
(PUT '|d| 'REDUCEVERSION 'DF) 
(PUT '|sIMPLIFY| 'REDUCEVERSION 'MMA_SIMPLIFY) 
(PUT '|aBS| 'REDUCEVERSION 'ABS) 
(PUT '|eXP| 'REDUCEVERSION 'EXP) 
(PUT '|lOG| 'REDUCEVERSION 'LOG) 
(PUT '|sQRT| 'REDUCEVERSION 'SQRT) 
(PUT '|sIN| 'REDUCEVERSION 'SIN) 
(PUT '|cOS| 'REDUCEVERSION 'COS) 
(PUT '|sEC| 'REDUCEVERSION 'SEC) 
(PUT '|cSC| 'REDUCEVERSION 'CSC) 
(PUT '|tAN| 'REDUCEVERSION 'TAN) 
(PUT '|cOT| 'REDUCEVERSION 'COT) 
(PUT '|sINH| 'REDUCEVERSION 'SINH) 
(PUT '|cOSH| 'REDUCEVERSION 'COSH) 
(PUT '|sECH| 'REDUCEVERSION 'SECH) 
(PUT '|cSCH| 'REDUCEVERSION 'CSCH) 
(PUT '|tANH| 'REDUCEVERSION 'TANH) 
(PUT '|cOTH| 'REDUCEVERSION 'COTH) 
(PUT '|aRCsIN| 'REDUCEVERSION 'ASIN) 
(PUT '|aRCcOS| 'REDUCEVERSION 'ACOS) 
(PUT '|aRCsEC| 'REDUCEVERSION 'ASEC) 
(PUT '|aRCcSC| 'REDUCEVERSION 'ACSC) 
(PUT '|aRCtAN| 'REDUCEVERSION 'ATAN) 
(PUT '|aRCcOT| 'REDUCEVERSION 'ACOT) 
(PUT '|aRCsINH| 'REDUCEVERSION 'ASINH) 
(PUT '|aRCcOSH| 'REDUCEVERSION 'ACOSH) 
(PUT '|aRCsECH| 'REDUCEVERSION 'ASECH) 
(PUT '|aRCcSCH| 'REDUCEVERSION 'ACSCH) 
(PUT '|aRCtANH| 'REDUCEVERSION 'ATANH) 
(PUT '|aRCcOTH| 'REDUCEVERSION 'ACOTH) 
(PUT '|eRF| 'REDUCEVERSION 'ERF) 
(PUT '|eLLIPTICe| 'REDUCEVERSION 'ELLIPTICE) 
(PUT '|eLLIPTICf| 'REDUCEVERSION 'ELLIPTICF) 
(PUT '|eLLIPTICpI| 'REDUCEVERSION 'ELLIPTICPI) 
(PUT '|eXPiNTEGRALeI| 'REDUCEVERSION 'EI) 
(PUT '|lOGiNTEGRAL| 'REDUCEVERSION 'LI) 
(PUT '|pOLYlOG| 'REDUCEVERSION 'POLYLOG) 
(PUT '|pRODUCTlOG| 'REDUCEVERSION 'PRODUCTLOG) 
(PUT '|eRFC| 'REDUCEVERSION 'ERFC) 
(PUT '|eRFI| 'REDUCEVERSION 'ERFI) 
(PUT '|fRESNELs| 'REDUCEVERSION 'FRESNEL_S) 
(PUT '|fRESNELc| 'REDUCEVERSION 'FRESNEL_C) 
(PUT '|sINiNTEGRAL| 'REDUCEVERSION 'SI) 
(PUT '|cOSiNTEGRAL| 'REDUCEVERSION 'CI) 
(PUT '|gAMMA| 'REDUCEVERSION 'GAMMA) 
(PUT '|pOLYgAMMA| 'REDUCEVERSION 'POLYGAMMA) 
(PUT '|lOGgAMMA| 'REDUCEVERSION 'LOGGAMMA) 
(PUT '|sINHiNTEGRAL| 'REDUCEVERSION 'SHI) 
(PUT '|cOSHiNTEGRAL| 'REDUCEVERSION 'CHI) 
(PUT '|zETA| 'REDUCEVERSION 'ZETA) 
(PROG (X)
  (SETQ X
          '(ELLIPTICE ELLIPTICF ELLIPTICPI LI POLYLOG PRODUCTLOG ERFC ERFI
            FRESNEL_S FRESNEL_C POLYGAMMA LOGGAMMA SHI CHI ZETA MMA_SIMPLIFY))
 LAB
  (COND ((NULL X) (RETURN NIL)))
  ((LAMBDA (X) (COND ((NOT (GET X 'SIMPFN)) (APPLY1 'OPERATOR (LIST X)))))
   (CAR X))
  (SETQ X (CDR X))
  (GO LAB)) 
(AEVAL (OPERATOR (LIST 'F '|cOMPLEXeXPAND|))) 
(AEVAL (FORALL (LIST '(X) 'T '(LET00 '((EQUAL (MMA_SIMPLIFY X) X)))))) 
(FLUID '(TESTCASES)) 
(SETQ TESTCASES NIL) 
(SETK 'TRIG_NORMALISATIONS
      (AEVAL
       (LIST 'LIST
             (LIST 'REPLACEBY (LIST 'ASIN (LIST '~ 'Z))
                   (LIST 'MINUS
                         (LIST 'TIMES 'I
                               (LIST 'LOG
                                     (LIST 'PLUS (LIST 'TIMES 'I 'Z)
                                           (LIST 'SQRT
                                                 (LIST 'DIFFERENCE 1
                                                       (LIST 'EXPT 'Z 2))))))))
             (LIST 'REPLACEBY (LIST 'ACOS (LIST '~ 'Z))
                   (LIST 'DIFFERENCE (LIST 'QUOTIENT 'PI 2) (LIST 'ASIN 'Z)))
             (LIST 'REPLACEBY (LIST 'ATAN (LIST '~ 'Z))
                   (LIST 'TIMES (LIST 'QUOTIENT 'I 2)
                         (LIST 'DIFFERENCE (LIST 'LOG (LIST 'DIFFERENCE 1 'Z))
                               (LIST 'LOG
                                     (LIST 'PLUS 1 (LIST 'TIMES 'I 'Z))))))
             (LIST 'REPLACEBY (LIST 'ACOT (LIST '~ 'Z))
                   (LIST 'ATAN (LIST 'QUOTIENT 1 'Z)))
             (LIST 'REPLACEBY (LIST 'ACSC (LIST '~ 'Z))
                   (LIST 'ASIN (LIST 'QUOTIENT 1 'Z)))
             (LIST 'REPLACEBY (LIST 'ASEC (LIST '~ 'Z))
                   (LIST 'ACOS (LIST 'QUOTIENT 1 'Z)))
             (LIST 'REPLACEBY (LIST 'ASINH (LIST '~ 'Z))
                   (LIST 'LOG
                         (LIST 'PLUS 'Z
                               (LIST 'SQRT (LIST 'PLUS 1 (LIST 'EXPT 'Z 2))))))
             (LIST 'REPLACEBY (LIST 'ACOSH (LIST '~ 'Z))
                   (LIST 'LOG
                         (LIST 'PLUS 'Z
                               (LIST 'TIMES (LIST 'SQRT (LIST 'PLUS 'Z 1))
                                     (LIST 'SQRT (LIST 'DIFFERENCE 'Z 1))))))
             (LIST 'REPLACEBY (LIST 'ATANH (LIST '~ 'Z))
                   (LIST 'QUOTIENT
                         (LIST 'LOG
                               (LIST 'QUOTIENT (LIST 'PLUS 1 'Z)
                                     (LIST 'DIFFERENCE 1 'Z)))
                         2))
             (LIST 'REPLACEBY (LIST 'ACOTH (LIST '~ 'Z))
                   (LIST 'QUOTIENT
                         (LIST 'LOG
                               (LIST 'QUOTIENT (LIST 'PLUS 'Z 1)
                                     (LIST 'DIFFERENCE 'Z 1)))
                         2))
             (LIST 'REPLACEBY (LIST 'ACSCH (LIST '~ 'Z))
                   (LIST 'LOG
                         (LIST 'PLUS (LIST 'QUOTIENT 1 'Z)
                               (LIST 'SQRT
                                     (LIST 'PLUS
                                           (LIST 'QUOTIENT 1 (LIST 'EXPT 'Z 2))
                                           1)))))
             (LIST 'REPLACEBY (LIST 'ASECH (LIST '~ 'Z))
                   (LIST 'LOG
                         (LIST 'PLUS (LIST 'QUOTIENT 1 'Z)
                               (LIST 'TIMES
                                     (LIST 'SQRT
                                           (LIST 'PLUS (LIST 'QUOTIENT 1 'Z)
                                                 1))
                                     (LIST 'SQRT
                                           (LIST 'DIFFERENCE
                                                 (LIST 'QUOTIENT 1 'Z) 1))))))
             (LIST 'REPLACEBY
                   (LIST 'EXPT 'E
                         (LIST 'QUOTIENT
                               (LIST 'TIMES (LIST '~ 'A)
                                     (LIST 'LOG (LIST '~ 'B)))
                               (LIST '~ 'C)))
                   (LIST 'EXPT 'B (LIST 'QUOTIENT 'A 'C)))))) 
(FLUID '(TIME_LIMIT*)) 
(SETQ TIME_LIMIT* 20000) 
(PUT 'SAFE_EVALUATE 'NUMBER-OF-ARGS 1) 
(PUT 'SAFE_EVALUATE 'DEFINED-ON-LINE '987) 
(PUT 'SAFE_EVALUATE 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'SAFE_EVALUATE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SAFE_EVALUATE (A)
    (PROG (W)
      (SETQ W (WITH-TIMEOUT TIME_LIMIT* (LIST 'ERRORSET (MKQUOTE A) NIL NIL)))
      (COND ((ATOM W) (RETURN 'TIMEOUT)))
      (RETURN (CAR W)))) 
(PUT 'TRY_RUBI_EXAMPLE 'NUMBER-OF-ARGS 2) 
(PUT 'TRY_RUBI_EXAMPLE 'DEFINED-ON-LINE '996) 
(PUT 'TRY_RUBI_EXAMPLE 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'TRY_RUBI_EXAMPLE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TRY_RUBI_EXAMPLE (R INTEGRAND)
    (PROG (RR E X STEPS RES W1 W2 FF)
      (SETQ RR R)
      (COND
       ((OR (NOT (EQCAR R 'BRACELIST))
            (AND (NEQ (LENGTH R) 5) (NEQ (LENGTH R) 6)))
        (PROGN
         (TERPRI)
         (PRINC "+++ Malformed test case: ")
         (PRETTYPRINT R)
         (RETURN NIL))))
      (COND
       ((AND *TRRUBI (NULL TESTCASES))
        (SETQ TESTCASES (OPEN "testcases.red" 'OUTPUT))))
      (SETQ R (CDR R))
      (PRINC "%%%%:::: ")
      (PROG (X)
        (SETQ X INTEGRAND)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (PRINC X)) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (TERPRI)
      (SETQ E (CONVERT_TO_REDUCE (CAR R)))
      (SETQ R (CDR R))
      (SETQ X (CAR R))
      (SETQ R (CDR R))
      (SETQ STEPS (CAR R))
      (SETQ R (CDR R))
      (SETQ RES (CONVERT_TO_REDUCE (CAR R)))
      (SETQ R (CDR R))
      (SETQ W1 (LIST 'PREPSQ (LIST 'SIMP* (MKQUOTE (LIST 'INT E X)))))
      (SETQ W1 (SAFE_EVALUATE W1))
      (PROGN (PRIN2 "Integration complete") (TERPRI) "Integration complete")
      (FLUSH NIL)
      (COND
       ((ATOM W1)
        (PROGN
         (COND
          (*TRRUBI
           (PROGN
            (SETQ FF (WRS TESTCASES))
            (COND
             ((EQUAL W1 'TIMEOUT)
              (PROGN (PRIN2 "% Timeout") (TERPRI) "% Timeout"))
             (T (PROGN (PRIN2 "% Errorset trap") (TERPRI) "% Errorset trap")))
            ((LAMBDA (*NAT)
               (PROGN
                (PRIN2* "COMMENT ")
                (MAPRIN (CONVERT_TO_REDUCE (CADR RR)))
                (PRIN2* ";")
                (TERPRI* T)))
             NIL)
            (PRINC "try_rubi_example '")
            (PRIN1 RR)
            (PROGN (PRIN2 ";") (TERPRI) ";")
            (TERPRI)
            (WRS FF)
            (FLUSH TESTCASES))))
         (COND
          ((EQUAL W1 'TIMEOUT) (PROGN (PRIN2 "Timeout") (TERPRI) "Timeout"))
          (T (PROGN (PRIN2 "Errorset trap") (TERPRI) "Errorset trap"))))))
      (COND
       ((OR (ATOM W1) (SMEMQ 'INT W1))
        (PROGN
         (MATHPRINT (LIST 'INT E X))
         (PROGN (PRIN2 "Unable to integrate") (TERPRI) "Unable to integrate")
         (MATHPRINT RES)
         (SETQ UNABLE (PLUS UNABLE 1))
         (RETURN NIL))))
      (SETQ W1 (CAR W1))
      (PROGN (PRIN2 "Integration done") (TERPRI) "Integration done")
      (SETQ W2 (PREPSQ (SIMP* (LIST 'DIFFERENCE E (LIST 'DF W1 X)))))
      (COND
       ((NEQ W2 0)
        (PROG (*PRECISE)
          (PROGN
           (PRIN2 "About to normalise trig fns")
           (TERPRI)
           "About to normalise trig fns")
          (PROGN (PRIN2 "Resimplify") (TERPRI) "Resimplify")
          (SETQ W2 (PREPSQ (SIMP* (LIST 'TRIGSIMP W2 'EXPON))))
          (COND
           ((NEQ W2 0)
            (PROGN
             (PRIN2 "normalise inverse trigs to logarithms")
             (TERPRI)
             "normalise inverse trigs to logarithms")))
          (COND
           ((NEQ W2 0)
            (SETQ W2
                    (PREPSQ
                     (SIMP*
                      (REVAL1 (LIST 'WHEREEXP '(LIST TRIG_NORMALISATIONS) W2)
                              NIL))))))
          (COND
           ((EQUAL W2 0)
            (PROGN
             (PRIN2 "Result seems OK at least in \"off precise\" mode")
             (TERPRI)
             "Result seems OK at least in \"off precise\" mode"))))))
      (COND
       ((NEQ W2 0)
        (PROGN
         (PRINC "Running probabilistic check on ")
         (PRINT W2)
         (COND ((PROBABLY_ZERO E W2) (SETQ W2 0))))))
      (COND
       ((NEQ W2 0)
        (PROGN
         (MATHPRINT (LIST 'INT E X))
         (PROGN
          (PRIN2 "Invalid result, or checking failed")
          (TERPRI)
          "Invalid result, or checking failed")
         (SETQ INVALID (PLUS INVALID 1))
         (COND
          (*TRRUBI
           (PROGN
            (SETQ E (WRS TESTCASES))
            (PROGN
             (PRIN2 "% Result may be incorrect")
             (TERPRI)
             "% Result may be incorrect")
            ((LAMBDA (*NAT)
               (PROGN
                (PRIN2* "COMMENT ")
                (MAPRIN (CONVERT_TO_REDUCE (CADR RR)))
                (PRIN2* ";")
                (TERPRI* T)))
             NIL)
            (PRINC "try_rubi_example '")
            (PRIN1 RR)
            (PROGN (PRIN2 ";") (TERPRI) ";")
            (TERPRI)
            (WRS E)
            (FLUSH TESTCASES))))
         (RETURN NIL))))
      (COND
       ((GREATERP (XSIZE W1) (TIMES 2 (XSIZE RES)))
        (PROGN
         (MATHPRINT (LIST 'INT E X))
         (PROGN
          (PRIN2 "Result is correct but messy")
          (TERPRI)
          "Result is correct but messy")
         (SETQ MESSY (PLUS MESSY 1))))
       (T (PROGN (SETQ OPTIMAL (PLUS OPTIMAL 1))))))) 
(PUT 'READ_ONE_RUBI_TEST 'NUMBER-OF-ARGS 2) 
(PUT 'READ_ONE_RUBI_TEST 'DEFINED-ON-LINE '1108) 
(PUT 'READ_ONE_RUBI_TEST 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_ONE_RUBI_TEST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE READ_ONE_RUBI_TEST (FILENAME VERSION4)
    (PROG (INTEGRAND R FF SAVE *ECHO)
      (COND
       ((OR (NOT (BOUNDP 'TESTDIRECTORY)) (NOT (STRINGP TESTDIRECTORY)))
        (PROGN
         (COND
          ((SETQ R (GETENV "reduce"))
           (SETQ TESTDIRECTORY (CONCAT R "/packages/rubi_red")))
          ((SETQ R (GETENV "O"))
           (SETQ TESTDIRECTORY (CONCAT R "/packages/rubi_red")))
          (T (SETQ TESTDIRECTORY "."))))))
      (SETQ R (EXPLODE2 TESTDIRECTORY))
      (SETQ SAVE '(/ C Y G D R I V E /))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND SAVE (EQCAR R (CAR SAVE)))) (RETURN NIL)))
        (PROGN (SETQ R (CDR R)) (SETQ SAVE (CDR SAVE)))
        (GO WHILELABEL))
      (COND
       ((AND (NULL SAVE) R (EQCAR (CDR R) '/))
        (PROGN
         (SETQ R (CONS (CAR R) (CONS '|:| (CDR R))))
         (SETQ TESTDIRECTORY (LIST2STRING R)))))
      (COND
       (VERSION4
        (SETQ FILENAME (CONCAT TESTDIRECTORY (CONCAT "/tests4/" FILENAME))))
       (T (SETQ FILENAME (CONCAT TESTDIRECTORY (CONCAT "/tests/" FILENAME)))))
      (SETQ FF (OPEN FILENAME 'INPUT))
      (SETQ SAVE (RDS FF))
      (SETQ PREVLINE (SETQ CURRLINE NIL))
      (SETQ MMACHAR $EOL$)
      (SETQ LINE_NUMBER 1)
      (SETQ EOFCOUNTER 0)
      (SETQ OUTER_LEVEL NIL)
      (NEXT_MMA_TOK)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ MMATOK $EOF$)) (RETURN NIL)))
        (PROGN
         (COND ((NEQ MMATOK '{) (SYNTAX_ERROR "'{' expected")))
         (SETQ CURREXPR (LIST (CAR CURREXPR)))
         (NEXT_MMA_TOK)
         (SETQ R (PARSEEXPRESSION))
         (SETQ INTEGRAND (REVERSE (CDDR CURREXPR)))
         (PROG ()
          WHILELABEL
           (COND ((NOT (EQCAR INTEGRAND $EOL$)) (RETURN NIL)))
           (SETQ INTEGRAND (CDR INTEGRAND))
           (GO WHILELABEL))
         (SETQ R (CONS 'BRACELIST (CONS R (PARSEBRACETAIL))))
         (NEXT_MMA_TOK)
         (WITH-TIMEOUT (TIMES 2 TIME_LIMIT*) (TRY_RUBI_EXAMPLE R INTEGRAND)))
        (GO WHILELABEL))
      (RDS SAVE)
      (CLOSE FF)
      (RETURN NIL))) 
(PUT 'READ_RUBI_RATIONALS 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_RATIONALS 'DEFINED-ON-LINE '1167) 
(PUT 'READ_RUBI_RATIONALS 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_RATIONALS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_RATIONALS NIL
    (PROGN
     (START_TEST_SECTION "Rational functions")
     (READ_ONE_RUBI_TEST "RationalFunctions/RationalFunctionsOfBinomials.m"
      NIL)
     (READ_ONE_RUBI_TEST "RationalFunctions/RationalFunctionsOfLinears.m" NIL)
     (READ_ONE_RUBI_TEST "RationalFunctions/RationalFunctionsOfTrinomials.m"
      NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_ALGEBRAICS 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_ALGEBRAICS 'DEFINED-ON-LINE '1175) 
(PUT 'READ_RUBI_ALGEBRAICS 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_ALGEBRAICS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_ALGEBRAICS NIL
    (PROGN
     (START_TEST_SECTION "Algebraic functions")
     (READ_ONE_RUBI_TEST "AlgebraicFunctions/AlgebraicFunctionsOfBinomials.m"
      NIL)
     (READ_ONE_RUBI_TEST "AlgebraicFunctions/AlgebraicFunctionsOfLinears.m"
      NIL)
     (READ_ONE_RUBI_TEST "AlgebraicFunctions/AlgebraicFunctionsOfTrinomials.m"
      NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_EXPONENTIALS 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_EXPONENTIALS 'DEFINED-ON-LINE '1183) 
(PUT 'READ_RUBI_EXPONENTIALS 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_EXPONENTIALS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_EXPONENTIALS NIL
    (PROGN
     (START_TEST_SECTION "Exponential functions")
     (AEVAL (OPERATOR (LIST 'F)))
     (READ_ONE_RUBI_TEST "ExponentialFunctions/ExponentialFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_LOGARITHMS 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_LOGARITHMS 'DEFINED-ON-LINE '1190) 
(PUT 'READ_RUBI_LOGARITHMS 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_LOGARITHMS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_LOGARITHMS NIL
    (PROGN
     (START_TEST_SECTION "Logarithmic functions")
     (AEVAL (OPERATOR (LIST '|cOMPLEXeXPAND|)))
     (READ_ONE_RUBI_TEST "ExponentialFunctions/LogarithmFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_TRIG 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_TRIG 'DEFINED-ON-LINE '1197) 
(PUT 'READ_RUBI_TRIG 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_TRIG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_TRIG NIL
    (PROGN
     (START_TEST_SECTION "Trig functions")
     (READ_ONE_RUBI_TEST "TrigFunctions/cos(x)^m (a+a cos(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/cos(x)^m (a+b cos(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/cos(x)^m (b cos(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/CosecantFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/CosineFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/CotangentFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/csc(x)^m (a+a csc(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/csc(x)^m (a+b csc(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/sec(x)^m (a+a sec(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/sec(x)^m (a+b sec(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/sec(x)^m (b sec(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/SecantFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/sin(x)^m (a+a sin(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/sin(x)^m (a+b sin(x))^n.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/SineFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/TangentFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/TrigFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "TrigFunctions/TwoTrigFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_INVERSE_TRIG 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_INVERSE_TRIG 'DEFINED-ON-LINE '1220) 
(PUT 'READ_RUBI_INVERSE_TRIG 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_INVERSE_TRIG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_INVERSE_TRIG NIL
    (PROGN
     (START_TEST_SECTION "Inverse trig functions")
     (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseCosecantFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseCosineFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseCotangentFunctions.m"
      NIL)
     (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseSecantFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseSineFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseTangentFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_HYPERBOLIC 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_HYPERBOLIC 'DEFINED-ON-LINE '1231) 
(PUT 'READ_RUBI_HYPERBOLIC 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_HYPERBOLIC 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_HYPERBOLIC NIL
    (PROGN
     (START_TEST_SECTION "Hyperbolic functions")
     (AEVAL (OPERATOR (LIST 'F)))
     (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicCosecantFunctions.m"
      NIL)
     (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicCosineFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicCotangentFunctions.m"
      NIL)
     (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicSecantFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicSineFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicTangentFunctions.m"
      NIL)
     (READ_ONE_RUBI_TEST "HyperbolicFunctions/TwoHyperbolicFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_INVERSE_HYPERBOLIC 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_INVERSE_HYPERBOLIC 'DEFINED-ON-LINE '1245) 
(PUT 'READ_RUBI_INVERSE_HYPERBOLIC 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_INVERSE_HYPERBOLIC 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_INVERSE_HYPERBOLIC NIL
    (PROGN
     (START_TEST_SECTION "Inverse hyperbolic functions")
     (READ_ONE_RUBI_TEST
      "InverseHyperbolicFunctions/InverseHyperbolicCosecantFunctions.m" NIL)
     (READ_ONE_RUBI_TEST
      "InverseHyperbolicFunctions/InverseHyperbolicCosineFunctions.m" NIL)
     (READ_ONE_RUBI_TEST
      "InverseHyperbolicFunctions/InverseHyperbolicCotangentFunctions.m" NIL)
     (READ_ONE_RUBI_TEST
      "InverseHyperbolicFunctions/InverseHyperbolicSecantFunctions.m" NIL)
     (READ_ONE_RUBI_TEST
      "InverseHyperbolicFunctions/InverseHyperbolicSineFunctions.m" NIL)
     (READ_ONE_RUBI_TEST
      "InverseHyperbolicFunctions/InverseHyperbolicTangentFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_ERROR_FRESNEL 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_ERROR_FRESNEL 'DEFINED-ON-LINE '1256) 
(PUT 'READ_RUBI_ERROR_FRESNEL 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_ERROR_FRESNEL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_ERROR_FRESNEL NIL
    (PROGN
     (START_TEST_SECTION "Error and Fresnel functions")
     (READ_ONE_RUBI_TEST "SpecialFunctions/ErrorFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "SpecialFunctions/FresnelIntegralFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_INTEGRAL 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_INTEGRAL 'DEFINED-ON-LINE '1263) 
(PUT 'READ_RUBI_INTEGRAL 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_INTEGRAL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_INTEGRAL NIL
    (PROGN
     (START_TEST_SECTION "Integral functions")
     (READ_ONE_RUBI_TEST "SpecialFunctions/ExponentialIntegralFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "SpecialFunctions/HyperbolicIntegralFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "SpecialFunctions/LogarithmIntegralFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "SpecialFunctions/TrigIntegralFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_RUBI_SPECIAL 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_SPECIAL 'DEFINED-ON-LINE '1272) 
(PUT 'READ_RUBI_SPECIAL 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_SPECIAL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_SPECIAL NIL
    (PROGN
     (START_TEST_SECTION "Special functions")
     (READ_ONE_RUBI_TEST "SpecialFunctions/GammaFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "SpecialFunctions/PolylogarithmFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "SpecialFunctions/ProductLogarithmFunctions.m" NIL)
     (READ_ONE_RUBI_TEST "SpecialFunctions/ZetaFunctions.m" NIL)
     (REPORT_SECTION)
     NIL)) 
(PUT 'READ_ALL_RUBI_TESTS 'NUMBER-OF-ARGS 0) 
(PUT 'READ_ALL_RUBI_TESTS 'DEFINED-ON-LINE '1281) 
(PUT 'READ_ALL_RUBI_TESTS 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_ALL_RUBI_TESTS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_ALL_RUBI_TESTS NIL
    (PROG (SECTION_NAME TESTCOUNT OPTIMAL MESSY UNABLE INVALID T_TESTCOUNT
           T_OPTIMAL T_MESSY T_UNABLE T_INVALID)
      (START_WHOLE_TEST)
      (READ_RUBI_RATIONALS)
      (READ_RUBI_ALGEBRAICS)
      (READ_RUBI_EXPONENTIALS)
      (READ_RUBI_LOGARITHMS)
      (READ_RUBI_TRIG)
      (READ_RUBI_INVERSE_TRIG)
      (READ_RUBI_HYPERBOLIC)
      (READ_RUBI_INVERSE_HYPERBOLIC)
      (READ_RUBI_ERROR_FRESNEL)
      (READ_RUBI_INTEGRAL)
      (READ_RUBI_SPECIAL)
      (FINAL_REPORT))) 
(PUT 'READ_ALL_RUBI_TESTS_VERSION4 'NUMBER-OF-ARGS 0) 
(PUT 'READ_ALL_RUBI_TESTS_VERSION4 'DEFINED-ON-LINE '1312) 
(PUT 'READ_ALL_RUBI_TESTS_VERSION4 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_ALL_RUBI_TESTS_VERSION4 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_ALL_RUBI_TESTS_VERSION4 NIL
    (PROG (SECTION_NAME TESTCOUNT OPTIMAL MESSY UNABLE INVALID T_TESTCOUNT
           T_OPTIMAL T_MESSY T_UNABLE T_INVALID)
      (START_WHOLE_TEST)
      (START_TEST_SECTION "Exponential Functions")
      (READ_ONE_RUBI_TEST "ExponentialFunctions/ExponentialFunctions.m" T)
      (READ_ONE_RUBI_TEST "ExponentialFunctions/LogarithmFunctions.m" T)
      (REPORT_SECTION)
      (START_TEST_SECTION "Inverse Trig Functions")
      (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseCotangentFunctions.m" T)
      (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseCosineFunctions.m" T)
      (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseSineFunctions.m" T)
      (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseCosecantFunctions.m" T)
      (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseTangentFunctions.m" T)
      (READ_ONE_RUBI_TEST "InverseTrigFunctions/InverseSecantFunctions.m" T)
      (REPORT_SECTION)
      (START_TEST_SECTION "Hyperbolic Functions")
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicCotangentFunctions.m"
       T)
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicCosecantFunctions.m"
       T)
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicTangentFunctions.m" T)
      (READ_ONE_RUBI_TEST
       "HyperbolicFunctions/cosh(c+d x)^m (a+b cosh(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicFunctions.m" T)
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/tanh^3.m" T)
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicSecantFunctions.m" T)
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicSineFunctions.m" T)
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/TwoHyperbolicFunctions.m" T)
      (READ_ONE_RUBI_TEST "HyperbolicFunctions/HyperbolicCosineFunctions.m" T)
      (REPORT_SECTION)
      (START_TEST_SECTION "Special Functions")
      (READ_ONE_RUBI_TEST "SpecialFunctions/TrigIntegralFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/GammaFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/PolylogarithmFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/HyperbolicIntegralFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/ProductLogarithmFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/ExponentialIntegralFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/ZetaFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/FresnelIntegralFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/LogarithmIntegralFunctions.m" T)
      (READ_ONE_RUBI_TEST "SpecialFunctions/ErrorFunctions.m" T)
      (REPORT_SECTION)
      (START_TEST_SECTION "P3 Trinomial Functions")
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.1.5 u (a+b x+c x^2)^n (d+e x+f x^2)^p.m" T)
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.3.2 x^m (d+e x^n)^q (a+b x^n+c x^(2n))^p.m"
       T)
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.3.1 x^m (a+b x^n+c x^(2n))^p.m" T)
      (READ_ONE_RUBI_TEST "P3 Trinomial Functions/P3.1.1 x^m (a+b x+c x^2)^p.m"
       T)
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.2.1 x^m (a+b x^2+c x^4)^p.m" T)
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.3.3 x^m (a x^q+b x^n+c x^(2 n-q))^p.m" T)
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.1.3 x^m (d+e x)^n (a+b x+c x^2)^p.m" T)
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.1.2 (d+e x)^m (a+b x+c x^2)^p.m" T)
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.2.2 x^m (d+e x^2)^n (a+b x^2+c x^4)^p.m" T)
      (READ_ONE_RUBI_TEST
       "P3 Trinomial Functions/P3.1.4 (d+e x)^m (f+g x)^n (a+b x+c x^2)^p.m" T)
      (REPORT_SECTION)
      (START_TEST_SECTION "Inverse Hyperbolic Functions")
      (READ_ONE_RUBI_TEST
       "InverseHyperbolicFunctions/InverseHyperbolicTangentFunctions.m" T)
      (READ_ONE_RUBI_TEST
       "InverseHyperbolicFunctions/InverseHyperbolicCosineFunctions.m" T)
      (READ_ONE_RUBI_TEST
       "InverseHyperbolicFunctions/InverseHyperbolicSineFunctions.m" T)
      (READ_ONE_RUBI_TEST
       "InverseHyperbolicFunctions/InverseHyperbolicCosecantFunctions.m" T)
      (READ_ONE_RUBI_TEST
       "InverseHyperbolicFunctions/InverseHyperbolicSecantFunctions.m" T)
      (READ_ONE_RUBI_TEST
       "InverseHyperbolicFunctions/InverseHyperbolicCotangentFunctions.m" T)
      (REPORT_SECTION)
      (START_TEST_SECTION "P1 Monomial Functions")
      (READ_ONE_RUBI_TEST "P1 Monomial Functions/P1.7 Miscellaneous problems.m"
       T)
      (READ_ONE_RUBI_TEST
       "P1 Monomial Functions/P1.1 x^m (a x^r+b x^s+...)^n.m" T)
      (READ_ONE_RUBI_TEST
       "P1 Monomial Functions/P1.5 Piecewise constant extraction problems.m" T)
      (READ_ONE_RUBI_TEST "P1 Monomial Functions/P1.6 Expansion problems.m" T)
      (READ_ONE_RUBI_TEST "P1 Monomial Functions/P1.3 Normalization problems.m"
       T)
      (READ_ONE_RUBI_TEST "P1 Monomial Functions/P1.2 x^m P[x] Q[x]^p.m" T)
      (READ_ONE_RUBI_TEST "P1 Monomial Functions/P1.4 Substitution problems.m"
       T)
      (REPORT_SECTION)
      (START_TEST_SECTION "Trig Functions")
      (READ_ONE_RUBI_TEST
       "TrigFunctions/8.1 trig(c+d x)^m (a cos(c+d x)+b sin(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/3.2 trig(a+b x)^m trig(c+d x)^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.5.2 sec(c+d x)^m (a+b sec(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.2.3 trig(c+d x)^m (a+b cos(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/8.4 trig(c+d x)^m (a+b trig(c+d x)^n)^p.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.1.1 sin(c+d x)^m (a+a sin(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/4.2 cos(a+b x)^m (c cos(a+b x))^n.m"
       T)
      (READ_ONE_RUBI_TEST "TrigFunctions/8.2 (c trig(a+b x)^m)^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.5.3 trig(c+d x)^m (a+b sec(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.2.2 cos(c+d x)^m (a+b cos(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.1.2 sin(c+d x)^m (a+b sin(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.1.3 trig(c+d x)^m (a+b sin(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.5.1 sec(c+d x)^m (a+a sec(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.3.3 trig(c+d x)^m (a+b tan(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.6.2 csc(c+d x)^m (a+b csc(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.6.3 trig(c+d x)^m (a+b csc(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/8.3 (a+b trig(c+d x)^m)^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.1.4 (A+B trig(c+d x)) (a+b sin(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/1. trig(a+b x)^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.6.1 csc(c+d x)^m (a+a csc(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.3.1 tan(c+d x)^m (a+i a tan(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/TrigFunctions.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/9.3 (d+e x)^m trig(a+b x+c x^2)^n.m"
       T)
      (READ_ONE_RUBI_TEST "TrigFunctions/9.2 x^m (a+b trig(c+d x)^n)^p.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/9.4 x^m trig(a+b log(c x^n))^p.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.4.2 cot(c+d x)^m (a+b cot(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/9.1 x^m trig(a+b x^n)^p.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.2.1 cos(c+d x)^m (a+a cos(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.3.2 tan(c+d x)^m (a+b tan(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.2.4 (A+B trig(c+d x)) (a+b cos(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST
       "TrigFunctions/7.4.3 trig(c+d x)^m (a+b cot(c+d x))^n.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/3.1 trig(a+b x)^m trig(a+b x)^n.m" T)
      (READ_ONE_RUBI_TEST "TrigFunctions/4.5 sec(a+b x)^m (c sec(a+b x))^n.m"
       T)
      (REPORT_SECTION)
      (START_TEST_SECTION "Miscellaneous Functions")
      (READ_ONE_RUBI_TEST "MiscellaneousFunctions/SymmetricModFunctions.m" T)
      (READ_ONE_RUBI_TEST "MiscellaneousFunctions/SchaumsIntegrationProblems.m"
       T)
      (READ_ONE_RUBI_TEST "MiscellaneousFunctions/zTrigProblems.m" T)
      (READ_ONE_RUBI_TEST "MiscellaneousFunctions/PiecewiseContinuouss.m" T)
      (READ_ONE_RUBI_TEST "MiscellaneousFunctions/TestGradnew.m" T)
      (READ_ONE_RUBI_TEST "MiscellaneousFunctions/newproblems.m" T)
      (READ_ONE_RUBI_TEST "MiscellaneousFunctions/DifferentiationFunctions.m"
       T)
      (READ_ONE_RUBI_TEST "MiscellaneousFunctions/SimpProblems.m" T)
      (REPORT_SECTION)
      (START_TEST_SECTION "P2 Binomial Functions")
      (READ_ONE_RUBI_TEST
       "P2 Binomial Functions/P2.1.3 (a+b x)^m (c+d x)^n (e+f x)^p.m" T)
      (READ_ONE_RUBI_TEST "P2 Binomial Functions/P2.3.1 x^m (a x^q+b x^n)^p.m"
       T)
      (READ_ONE_RUBI_TEST
       "P2 Binomial Functions/P2.2.2 (a+b x^n)^m (c+d x^n)^p.m" T)
      (READ_ONE_RUBI_TEST
       "P2 Binomial Functions/P2.1.2 (a+b x)^m (A+B x) (d+e x)^p.m" T)
      (READ_ONE_RUBI_TEST "P2 Binomial Functions/P2.2.5 x^m P[x] (a+b x^n)^p.m"
       T)
      (READ_ONE_RUBI_TEST
       "P2 Binomial Functions/P2.2.3 x^m (a+b x^n)^p (c+d x^n)^q.m" T)
      (READ_ONE_RUBI_TEST "P2 Binomial Functions/P2.2.1 x^m (a+b x^n)^p.m" T)
      (READ_ONE_RUBI_TEST "P2 Binomial Functions/P2.1.1 (a+b x)^m (c+d x)^n.m"
       T)
      (READ_ONE_RUBI_TEST
       "P2 Binomial Functions/P2.2.4 (a+b x^n)^m (c+d x^n)^p (e+f x^n)^q.m" T)
      (REPORT_SECTION)
      (START_TEST_SECTION "Contributed Problems")
      (READ_ONE_RUBI_TEST "ContributedProblems.m" T)
      (REPORT_SECTION)
      (FINAL_REPORT))) 
(FLUID '(RUBI_RULES)) 
(PUT 'READ_RUBI_RULE_FILE 'NUMBER-OF-ARGS 1) 
(PUT 'READ_RUBI_RULE_FILE 'DEFINED-ON-LINE '1465) 
(PUT 'READ_RUBI_RULE_FILE 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_RULE_FILE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE READ_RUBI_RULE_FILE (FILENAME)
    (PROG (R FF SAVE *ECHO)
      (SETQ FILENAME (CONCAT "$reduce/packages/rubi_red/rubi-rules/" FILENAME))
      (SETQ FF (OPEN FILENAME 'INPUT))
      (SETQ SAVE (RDS FF))
      (PRINC "Reading file ")
      (PRINT FILENAME)
      (SETQ PREVLINE (SETQ CURRLINE NIL))
      (SETQ MMACHAR $EOL$)
      (SETQ LINE_NUMBER 1)
      (SETQ EOFCOUNTER 0)
      (SETQ OUTER_LEVEL T)
      (NEXT_MMA_TOK)
      (SETQ R (ERRORSET '(PARSERULES) T T))
      (RDS SAVE)
      (CLOSE FF)
      (COND ((ATOM R) (RETURN (ERROR 0 "parsing failed"))))
      (SETQ R (CAR R))
      (PRINC "File processed giving ")
      (PRINC (LENGTH R))
      (PROGN (PRIN2 " rules") (TERPRI) " rules")
      (SETQ RUBI_RULES (APPEND RUBI_RULES R))
      (RETURN NIL))) 
(PUT 'READ_RUBI_RULES 'NUMBER-OF-ARGS 0) 
(PUT 'READ_RUBI_RULES 'DEFINED-ON-LINE '1493) 
(PUT 'READ_RUBI_RULES 'DEFINED-IN-FILE 'RUBI_RED/RUBI_PARSE.RED) 
(PUT 'READ_RUBI_RULES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_RUBI_RULES NIL
    (PROG (RUBI_RULES)
      (READ_RUBI_RULE_FILE
       "AlgebraicFunctions/AlgebraicFunctionsOfBinomials.m")
      (READ_RUBI_RULE_FILE "AlgebraicFunctions/AlgebraicFunctionsOfLinears.m")
      (READ_RUBI_RULE_FILE
       "AlgebraicFunctions/AlgebraicFunctionsOfTrinomials.m")
      (READ_RUBI_RULE_FILE "ExponentialFunctions/ExponentialFunctions.m")
      (READ_RUBI_RULE_FILE "ExponentialFunctions/LogarithmFunctions.m")
      (READ_RUBI_RULE_FILE
       "ExponentialFunctions/ProductsOfExponentialAndHyperbolicFunctions.m")
      (READ_RUBI_RULE_FILE
       "ExponentialFunctions/ProductsOfExponentialAndTrigFunctions.m")
      (READ_RUBI_RULE_FILE "HyperbolicFunctions/HyperbolicSecantFunctions.m")
      (READ_RUBI_RULE_FILE "HyperbolicFunctions/HyperbolicSineFunctions.m")
      (READ_RUBI_RULE_FILE "HyperbolicFunctions/HyperbolicSubstitution.m")
      (READ_RUBI_RULE_FILE "HyperbolicFunctions/HyperbolicTangentFunctions.m")
      (READ_RUBI_RULE_FILE
       "HyperbolicFunctions/RationalFunctionsOfHyperbolicSinesAndCosines.m")
      (READ_RUBI_RULE_FILE "HyperbolicFunctions/TwoHyperbolicFunctions.m")
      (READ_RUBI_RULE_FILE
       "InverseHyperbolicFunctions/InverseHyperbolicCosecantFunctions.m")
      (READ_RUBI_RULE_FILE
       "InverseHyperbolicFunctions/InverseHyperbolicCosineFunctions.m")
      (READ_RUBI_RULE_FILE
       "InverseHyperbolicFunctions/InverseHyperbolicCotangentFunctions.m")
      (READ_RUBI_RULE_FILE
       "InverseHyperbolicFunctions/InverseHyperbolicSecantFunctions.m")
      (READ_RUBI_RULE_FILE
       "InverseHyperbolicFunctions/InverseHyperbolicSineFunctions.m")
      (READ_RUBI_RULE_FILE
       "InverseHyperbolicFunctions/InverseHyperbolicTangentFunctions.m")
      (READ_RUBI_RULE_FILE "InverseTrigFunctions/InverseCosecantFunctions.m")
      (READ_RUBI_RULE_FILE "InverseTrigFunctions/InverseCosineFunctions.m")
      (READ_RUBI_RULE_FILE "InverseTrigFunctions/InverseCotangentFunctions.m")
      (READ_RUBI_RULE_FILE "InverseTrigFunctions/InverseSecantFunctions.m")
      (READ_RUBI_RULE_FILE "InverseTrigFunctions/InverseSineFunctions.m")
      (READ_RUBI_RULE_FILE "InverseTrigFunctions/InverseTangentFunctions.m")
      (READ_RUBI_RULE_FILE "RationalFunctions/RationalFunctionsOfBinomials.m")
      (READ_RUBI_RULE_FILE "RationalFunctions/RationalFunctionsOfLinears.m")
      (READ_RUBI_RULE_FILE "RationalFunctions/RationalFunctionsOfTrinomials.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/ErrorFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/ExponentialIntegralFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/FresnelIntegralFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/GammaFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/HyperbolicIntegralFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/LogarithmIntegralFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/PolylogarithmFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/ProductLogarithmFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/TrigIntegralFunctions.m")
      (READ_RUBI_RULE_FILE "SpecialFunctions/ZetaFunctions.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/(sin^j)^m (a+a sin^k)^n.m")
      (READ_RUBI_RULE_FILE
       "TrigFunctions/(sin^j)^m (A+B sin^k) (a+a sin^k)^n.m")
      (READ_RUBI_RULE_FILE
       "TrigFunctions/(sin^j)^m (A+B sin^k) (a+b sin^k)^n.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/(sin^j)^m (a+b sin^k)^n.m")
      (READ_RUBI_RULE_FILE
       "TrigFunctions/(sin^j)^m (A+B sin^k+C sin^(2k)) (a+a sin^k)^n.m")
      (READ_RUBI_RULE_FILE
       "TrigFunctions/(sin^j)^m (A+B sin^k+C sin^(2k)) (a+b sin^k)^n.m")
      (READ_RUBI_RULE_FILE
       "TrigFunctions/RationalFunctionsOfSinesAndCosines.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/RecurrenceEquations.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/SecantFunctions.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/SineFunctions.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/TangentFunctions.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/TrigNormalization.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/TrigSubstitution.m")
      (READ_RUBI_RULE_FILE "TrigFunctions/TwoTrigFunctions.m")
      ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X))
       (LIST "there are" (LENGTH RUBI_RULES) "rules"))
      (RETURN RUBI_RULES))) 
(ENDMODULE) 