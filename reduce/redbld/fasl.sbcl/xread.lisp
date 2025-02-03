(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'XREAD)) 
(FLUID '(*BLOCKP *EOLDELIMP *REDUCE4 COMMENTLIST*)) 
(GLOBAL '(CURSYM* CURESCAPED* NXTSYM*)) 
(FLAG '(END *COLON* *SEMICOL*) 'DELIM) 
(PUT 'CHKNEWNAM 'NUMBER-OF-ARGS 1) 
(PUT 'CHKNEWNAM 'DEFINED-ON-LINE '47) 
(PUT 'CHKNEWNAM 'DEFINED-IN-FILE 'RLISP/XREAD.RED) 
(PUT 'CHKNEWNAM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHKNEWNAM (U)
    (PROG (X)
      (RETURN
       (COND ((OR (NULL (SETQ X (GET U 'NEWNAM))) (EQ X U)) U)
             ((IDP X) (CHKNEWNAM X)) (T X))))) 
(PUT 'MKVAR 'NUMBER-OF-ARGS 2) 
(PUT 'MKVAR 'DEFINED-ON-LINE '60) 
(PUT 'MKVAR 'DEFINED-IN-FILE 'RLISP/XREAD.RED) 
(PUT 'MKVAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKVAR (U V) U) 
(PUT 'MKVAR 'INLINE '(LAMBDA (U V) U)) 
(PUT 'REMCOMMA 'NUMBER-OF-ARGS 1) 
(PUT 'REMCOMMA 'DEFINED-ON-LINE '63) 
(PUT 'REMCOMMA 'DEFINED-IN-FILE 'RLISP/XREAD.RED) 
(PUT 'REMCOMMA 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMCOMMA (U) (COND ((EQCAR U '*COMMA*) (CDR U)) (T (LIST U)))) 
(PUT 'EOLCHECK 'NUMBER-OF-ARGS 0) 
(PUT 'EOLCHECK 'DEFINED-ON-LINE '66) 
(PUT 'EOLCHECK 'DEFINED-IN-FILE 'RLISP/XREAD.RED) 
(PUT 'EOLCHECK 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EOLCHECK NIL
    (COND ((NULL *EOLDELIMP) NIL)
          (T
           (PROG ()
            WHILELABEL
             (COND ((NOT (EQ NXTSYM* $EOL$)) (RETURN NIL)))
             (SETQ NXTSYM* (COND ((EQUAL CURSYM* 'END) '|;|) (T (TOKEN))))
             (GO WHILELABEL))))) 
(PUT 'XCOMMENT 'NUMBER-OF-ARGS 2) 
(PUT 'XCOMMENT 'DEFINED-ON-LINE '72) 
(PUT 'XCOMMENT 'DEFINED-IN-FILE 'RLISP/XREAD.RED) 
(PUT 'XCOMMENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE XCOMMENT (U COMMENTLIST)
    (COND (COMMENTLIST (CONS 'COMMENT (ACONC (REVERSIP COMMENTLIST) U))) (T U))) 
(PUT 'XREAD1 'NUMBER-OF-ARGS 1) 
(PUT 'XREAD1 'DEFINED-ON-LINE '84) 
(PUT 'XREAD1 'DEFINED-IN-FILE 'RLISP/XREAD.RED) 
(PUT 'XREAD1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XREAD1 (U)
    (PROG (V W X Y Z Z1 Z2 COMMENTLIST)
      (COND
       (COMMENTLIST*
        (PROGN (SETQ COMMENTLIST COMMENTLIST*) (SETQ COMMENTLIST* NIL))))
     A
      (SETQ Z CURSYM*)
     A1
      (COND ((NULL (IDP Z)) NIL) ((EQUAL Z '*LPAR*) (GO LPAREN))
            ((EQUAL Z '*RPAR*) (GO RPAREN)) ((SETQ Y (GET Z 'INFIX)) (GO INFX))
            ((FLAGP Z 'DELIM) (GO DELIMIT)) ((SETQ Y (GET Z 'STAT)) (GO STAT))
            ((AND (NULL *REDUCE4) (FLAGP Z 'TYPE))
             (PROGN (SETQ W (CONS (LISPAPPLY 'DECSTAT NIL) W)) (GO A))))
     A2
      (SETQ Y NIL)
     A3
      (SETQ W (CONS Z W))
      (COND
       ((AND (TOKNUMP Z) (NULL (EQ Z1 $EOL$))
             (IDP (SETQ Z1 (CHKNEWNAM NXTSYM*))) (NULL (FLAGP Z1 'DELIM))
             (NULL (AND (GET Z1 'SWITCH*) (NULL (EQUAL Z1 '|(|))))
             (NULL (GET Z1 'INFIX)) (NULL (AND *EOLDELIMP (EQ Z1 $EOL$))))
        (PROGN (SETQ CURSYM* 'TIMES) (SETQ CURESCAPED* NIL) (GO A)))
       ((AND (EQUAL U 'PROC) (GREATERP (LENGTH W) 2))
        (SYMERR "Syntax error in procedure header" NIL)))
     NEXT
      (SETQ Z (SCAN))
      (GO A1)
     LPAREN
      (EOLCHECK)
      (SETQ Y NIL)
      (COND ((EQUAL (SCAN) '*RPAR*) (GO LP1))
            ((FLAGPCAR W 'STRUCT) (SETQ Z (XREAD1 (CAR W))))
            (T (SETQ Z (XREAD1 'PAREN))))
      (COND ((FLAGP U 'STRUCT) (PROGN (SETQ Z (REMCOMMA Z)) (GO A3)))
            ((NULL (EQCAR Z '*COMMA*)) (GO A3))
            ((NULL W)
             (COND ((EQUAL U 'LAMBDA) (GO A3))
                   (T (SYMERR "Improper delimiter" NIL))))
            (T (SETQ W (CONS (CONS (CAR W) (CDR Z)) (CDR W)))))
      (GO NEXT)
     LP1
      (COND (W (SETQ W (CONS (LIST (CAR W)) (CDR W)))))
      (GO NEXT)
     RPAREN
      (COND
       ((OR (NULL U) (EQUAL U 'GROUP) (EQUAL U 'PROC))
        (SYMERR "Too many right parentheses" NIL))
       (T (GO END1)))
     INFX
      (EOLCHECK)
      (COND
       ((OR (EQUAL Z '*COMMA*) (NULL (ATOM (SETQ Z1 (SCAN)))) (TOKNUMP Z1))
        (GO IN1))
       ((OR (EQUAL Z1 '*RPAR*) (EQUAL Z1 '*COMMA*) (FLAGP Z1 'DELIM)) (GO IN2))
       ((AND (EQUAL Z1 '*LPAR*) (NULL (EOLCHECK))
             (NULL (ATOM (SETQ Z1 (XREAD 'PAREN)))) (EQUAL (CAR Z1) '*COMMA*)
             (SETQ Z (CONS Z (CDR Z1))))
        (GO A1)))
     IN1
      (COND (W (GO UNWIND))
            ((NULL (SETQ Z (GET Z 'UNARY))) (SYMERR "Redundant operator" NIL)))
      (SETQ V (CONS '**UN** V))
      (GO PR1)
     IN2
      (COND (Y (SETQ Y NIL)))
      (SETQ W (CONS Z W))
     IN3
      (SETQ Z Z1)
      (GO A1)
     UNWIND
      (COND ((NULL W) (SYMERR "Improper delimiter" NIL)))
      (SETQ Z2 (CAR W))
     UN1
      (SETQ W (CDR W))
      (COND ((NULL W) (GO UN2))
            ((AND (ATOM (CAR W)) (NULL (IDP (CAR W))))
             (SYMERR "Missing operator" NIL)))
      (SETQ Z2 (LIST (CAR W) Z2))
      (GO UN1)
     UN2
      (SETQ V (CONS Z2 V))
     PRECED
      (COND ((NULL X) (COND ((EQUAL Y 0) (GO END2)) (T NIL)))
            ((AND V (EQCAR (CDR V) '**UN**) (CDR X) (GEQ Y (CAAR X))
                  (LEQ Y (CAADR X)))
             (SYMERR "Please use parentheses around use of the unary operator"
                     NIL))
            ((OR (LESSP Y (CAAR X))
                 (AND (EQUAL Y (CAAR X))
                      (OR
                       (AND (EQ Z (CDAR X)) (NULL (FLAGP Z 'NARY))
                            (NULL (FLAGP Z 'RIGHT)))
                       (GET (CDAR X) 'ALT))))
             (GO PR2)))
     PR1
      (SETQ X (CONS (CONS Y Z) X))
      (COND ((NULL (EQUAL Z '*COMMA*)) (GO IN3))
            ((OR (CDR X) (NULL U) (MEMQ U '(LAMBDA PAREN)) (FLAGP U 'STRUCT))
             (GO NEXT))
            (T (GO END2)))
     PR2
      (COND
       ((AND (EQCAR (CADR V) 'NOT) (GEQ (CAAR X) (GET 'MEMBER 'INFIX)))
        (TYPERR "NOT" "infix operator")))
      (COND
       ((EQUAL (CADR V) '**UN**)
        (COND ((EQUAL (CAR V) '**UN**) (GO PR1))
              (T (SETQ Z2 (LIST (CDAR X) (CAR V))))))
       (T
        (SETQ Z2
                (CONS (CDAR X)
                      (COND
                       ((AND (EQCAR (CAR V) (CDAR X)) (FLAGP (CDAR X) 'NARY))
                        (CONS (CADR V) (CDAR V)))
                       (T (LIST (CADR V) (CAR V))))))))
      (SETQ X (CDR X))
      (SETQ V (CONS Z2 (CDDR V)))
      (GO PRECED)
     STAT
      (COND ((NULL (EQUAL Y 'ENDSTAT)) (EOLCHECK)))
      (COND
       ((NULL
         (OR (FLAGP Z 'GO)
             (AND (NULL (EQUAL U 'PROC))
                  (OR (FLAGP Y 'ENDSTATFN)
                      (AND (NULL (FLAGP NXTSYM* 'DELCHAR))
                           (NULL (EQUAL NXTSYM* '|,|)))))))
        (GO A2)))
      (COND
       ((AND (EQUAL Z 'PROCEDURE) *REDUCE4)
        (COND
         (W
          (COND ((OR (CDR W) *REDUCE4) (SYMERR "proc form" NIL))
                (T (SETQ W (LIST (PROCSTAT1 (CAR W)))))))
         (T (SETQ W (LIST (PROCSTAT1 NIL))))))
       (T (SETQ W (CONS (LISPAPPLY Y NIL) W))))
      (SETQ Y NIL)
      (GO A)
     DELIMIT
      (COND ((NULL (EQUAL CURSYM* '*SEMICOL*)) (EOLCHECK)))
      (COND
       ((OR
         (AND (EQUAL Z '*COLON*) (NULL (EQUAL U 'FOR))
              (OR (NULL *BLOCKP) (NULL W) (NULL (ATOM (CAR W))) (CDR W)))
         (AND (FLAGP Z 'NODEL)
              (OR (NULL U)
                  (AND (EQUAL U 'GROUP)
                       (NULL (MEMQ Z '(*ENDGROUP* *RCBKT* *RSQBKT*)))))))
        (SYMERR "Improper delimiter" NIL))
       ((AND (IDP U) (OR (EQUAL U 'PAREN) (FLAGP U 'STRUCT)))
        (SYMERR "Too few right parentheses" NIL)))
     END1
      (COND (Y (SYMERR "Improper delimiter" NIL))
            ((AND (NULL V) (NULL W) (NULL X))
             (RETURN (XCOMMENT NIL COMMENTLIST))))
      (SETQ Y 0)
      (GO UNWIND)
     END2
      (COND ((NULL (CDR V)) (RETURN (XCOMMENT (CAR V) COMMENTLIST)))
            (T (PRINT "Please send hearn@rand.org your program!!")))
      (SYMERR "Improper delimiter" NIL))) 
(FLAG '(ENDSTAT ENDSTAT1 RETSTAT) 'ENDSTATFN) 
(FLAG '(ELSE THEN UNTIL) 'NODEL) 
(FLAG '(BEGIN) 'GO) 
(PUT 'XREAD 'NUMBER-OF-ARGS 1) 
(PUT 'XREAD 'DEFINED-ON-LINE '299) 
(PUT 'XREAD 'DEFINED-IN-FILE 'RLISP/XREAD.RED) 
(PUT 'XREAD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XREAD (U)
    (PROG ()
      (PROG ()
       WHILELABEL
        (COND
         ((NOT (PROGN (SCAN) (AND *EOLDELIMP (EQUAL CURSYM* '*SEMICOL*))))
          (RETURN NIL)))
       NIL
        (GO WHILELABEL))
      (RETURN (XREAD1 U)))) 
(PUT 'EXPREAD 'NUMBER-OF-ARGS 0) 
(PUT 'EXPREAD 'DEFINED-ON-LINE '306) 
(PUT 'EXPREAD 'DEFINED-IN-FILE 'RLISP/XREAD.RED) 
(PUT 'EXPREAD 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EXPREAD NIL (XREAD T)) 
(FLAG '(EXPREAD XREAD) 'OPFN) 
(ENDMODULE) 