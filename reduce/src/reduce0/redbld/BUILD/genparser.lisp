(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(FLUID '(SYMBOLS NONTERMINALS LEX_CONTEXT PRECEDENCE_TABLE)) 
(PUT 'LALR_PRECEDENCE 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_PRECEDENCE 'DEFINED-ON-LINE '75) 
(PUT 'LALR_PRECEDENCE 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_PRECEDENCE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_PRECEDENCE (TERMINAL)
    ((LAMBDA (X) (COND (X (CAR X)) (T NIL))) (GETV PRECEDENCE_TABLE TERMINAL))) 
(PUT 'LALR_ASSOCIATIVITY 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_ASSOCIATIVITY 'DEFINED-ON-LINE '79) 
(PUT 'LALR_ASSOCIATIVITY 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_ASSOCIATIVITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_ASSOCIATIVITY (TERMINAL)
    ((LAMBDA (X) (COND (X (CDR X)) (T NIL))) (GETV PRECEDENCE_TABLE TERMINAL))) 
(FLUID '(TERMINAL_CODES NONTERMINAL_CODES)) 
(FLUID '(REDUCTION_INFO)) 
(FLUID '(ITEMSET_COLLECTION GOTO_TABLE)) 
(PUT 'LALR_ADD_GOTO 'NUMBER-OF-ARGS 3) 
(PUT 'LALR_ADD_GOTO 'DEFINED-ON-LINE '116) 
(PUT 'LALR_ADD_GOTO 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_ADD_GOTO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LALR_ADD_GOTO (SRC X DEST)
    (PUTHASH X GOTO_TABLE (CONS (CONS SRC DEST) (GETHASH X GOTO_TABLE)))) 
(PUT 'LALR_GOTO 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_GOTO 'DEFINED-ON-LINE '119) 
(PUT 'LALR_GOTO 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_GOTO 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_GOTO (SRC X)
    (PROG (RESULT_I_ITEMSET)
      (COND
       ((SETQ RESULT_I_ITEMSET (ASSOC SRC (GETHASH X GOTO_TABLE)))
        (RETURN (CDR RESULT_I_ITEMSET)))
       (T (RETURN NIL))))) 
(PUT 'LALR_PRODUCTIONS 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_PRODUCTIONS 'DEFINED-ON-LINE '150) 
(PUT 'LALR_PRODUCTIONS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_PRODUCTIONS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_PRODUCTIONS (X) (GET X 'LALR_PRODUCES)) 
(PUT 'LALR_CREATE_PARSER 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_CREATE_PARSER 'DEFINED-ON-LINE '270) 
(PUT 'LALR_CREATE_PARSER 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_CREATE_PARSER 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_CREATE_PARSER (PRECEDENCE_LIST GRAMMAR)
    (PROG (SYMBOLS NONTERMINALS LEX_CONTEXT PRECEDENCE_TABLE TERMINAL_CODES
           ITEMSET_COLLECTION GOTO_TABLE REDUCTION_INFO COMPRESSED_ACTION_TABLE
           COMPRESSED_GOTO_TABLE)
      (LALR_SET_GRAMMAR PRECEDENCE_LIST (LALR_EXPAND_GRAMMAR GRAMMAR))
      (SETQ GOTO_TABLE (MKHASH (LENGTH SYMBOLS) 3 1.5))
      (LALR_GENERATE_LR0_COLLECTION)
      (LALR_GENERATE_COLLECTION)
      (SETQ REDUCTION_INFO (LALR_PROCESS_REDUCTIONS))
      (SETQ COMPRESSED_ACTION_TABLE (LALR_MAKE_COMPRESSED_ACTION_TABLE))
      (SETQ COMPRESSED_GOTO_TABLE (LALR_MAKE_COMPRESSED_GOTO_TABLE))
      (LALR_CLEANUP)
      (RETURN
       (LIST LEX_CONTEXT COMPRESSED_ACTION_TABLE REDUCTION_INFO
             COMPRESSED_GOTO_TABLE NONTERMINAL_CODES TERMINAL_CODES)))) 
(PUT 'LALR_CLEANUP 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_CLEANUP 'DEFINED-ON-LINE '309) 
(PUT 'LALR_CLEANUP 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_CLEANUP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_CLEANUP NIL
    (PROG (SYMBOL)
      (SETQ SYMBOL SYMBOLS)
     LAB
      (COND ((NULL SYMBOL) (RETURN NIL)))
      ((LAMBDA (SYMBOL)
         (COND
          ((IDP SYMBOL)
           (PROGN
            (PUT SYMBOL 'LALR_PRODUCES NIL)
            (PUT SYMBOL 'LALR_FIRST NIL)
            (PUT SYMBOL 'LALR_NONTERMINAL_CODE NIL)))))
       (CAR SYMBOL))
      (SETQ SYMBOL (CDR SYMBOL))
      (GO LAB))) 
(FLUID '(PENDING_RULES*)) 
(PUT 'LALR_EXTRACT_NONTERMINALS 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_EXTRACT_NONTERMINALS 'DEFINED-ON-LINE '324) 
(PUT 'LALR_EXTRACT_NONTERMINALS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_EXTRACT_NONTERMINALS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_EXTRACT_NONTERMINALS (R)
    (COND ((NULL R) NIL)
          ((ATOM R)
           (REDERR
            (LIST "Malformed production" R
                  "(RHS should be a list of tokens, not a non-nil atom)")))
          ((STRINGP (CAR R)) (LALR_EXTRACT_NONTERMINALS (CDR R)))
          ((IDP (CAR R))
           (PROGN
            (COND
             ((GET (CAR R) 'LEX_FIXED_CODE)
              (LALR_EXTRACT_NONTERMINALS (CDR R)))
             (T (CONS (CAR R) (LALR_EXTRACT_NONTERMINALS (CDR R)))))))
          ((ATOM (CAR R))
           (REDERR
            (LIST "Malformed production" R
                  "(atomic item in token list should be symbol or string)")))
          ((MEMQ (CAAR R) '(OPT SEQ STAR PLUS LIST LISTPLUS OR))
           (APPEND (LALR_EXTRACT_NONTERMINALS (CDAR R))
                   (LALR_EXTRACT_NONTERMINALS (CDR R))))
          (T
           (REDERR
            (LIST "Malformed production" R "(unrecognised item in rule)"))))) 
(PUT 'LALR_CHECK_GRAMMAR 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_CHECK_GRAMMAR 'DEFINED-ON-LINE '344) 
(PUT 'LALR_CHECK_GRAMMAR 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_CHECK_GRAMMAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_CHECK_GRAMMAR (G)
    (PROG (R Q W W1)
      (COND ((NULL G) (REDERR "Empty grammar is illegal")))
      (PROG (X)
        (SETQ X G)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        (COND
         ((ATOM (CAR X))
          (REDERR (LIST "Improper item" (CAR X) "at top level in grammar")))
         ((NOT (IDP (CAAR X)))
          (REDERR
           (LIST "Malformed grammar rule" (CAR X)
                 "does not start with identifier")))
         ((ASSOC (CAAR X) (CDR X))
          (REDERR (LIST "Non-terminal" (CAAR X) "repeated in grammar")))
         ((OR (ATOM (CDAR X)) (ATOM (CADAR X)))
          (REDERR
           (LIST "Malformed grammar rule" (CAR X)
                 "has empty right hand side"))))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ Q (LIST (CAR G)))
      (SETQ R NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT Q) (RETURN NIL)))
        (PROGN
         (SETQ W (CAR Q))
         (SETQ Q (CDR Q))
         (SETQ R (CONS W R))
         (PROG (P)
           (SETQ P (CDR W))
          LAB
           (COND ((NULL P) (RETURN NIL)))
           ((LAMBDA (P)
              (PROGN
               (PROG (S)
                 (SETQ S (LALR_EXTRACT_NONTERMINALS (CAR P)))
                LAB
                 (COND ((NULL S) (RETURN NIL)))
                 ((LAMBDA (S)
                    (PROGN
                     (SETQ W1 (ASSOC S G))
                     (COND
                      ((NULL W1)
                       (REDERR
                        (LIST "Symbol" S "used in grammar but not defined"))))
                     (COND
                      ((AND (NOT (ASSOC S R)) (NOT (ASSOC S Q)))
                       (SETQ Q (CONS W1 Q))))))
                  (CAR S))
                 (SETQ S (CDR S))
                 (GO LAB))))
            (CAR P))
           (SETQ P (CDR P))
           (GO LAB)))
        (GO WHILELABEL))
      (PROG (X)
        (SETQ X R)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ G (DELASC (CAR X) G))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       ((AND G *MSG)
        (PROGN (LPRIM "Unused clauses in grammar:") (PRETTYPRINT G))))
      (RETURN (REVERSIP R)))) 
(PUT 'LALR_EXPAND_GRAMMAR 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_EXPAND_GRAMMAR 'DEFINED-ON-LINE '394) 
(PUT 'LALR_EXPAND_GRAMMAR 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_EXPAND_GRAMMAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_EXPAND_GRAMMAR (G)
    (PROG (PENDING_RULES* W R)
      (SETQ G (LALR_CHECK_GRAMMAR G))
      (SETQ PENDING_RULES* G)
      (SETQ R NIL)
      (PROG ()
       WHILELABEL
        (COND ((NOT PENDING_RULES*) (RETURN NIL)))
        (PROGN
         (SETQ W (CAR PENDING_RULES*))
         (SETQ PENDING_RULES* (CDR PENDING_RULES*))
         (SETQ R (CONS (EXPAND_RULE W) R)))
        (GO WHILELABEL))
      (RETURN (REVERSE R)))) 
(PUT 'EXPAND_RULE 'NUMBER-OF-ARGS 1) 
(PUT 'EXPAND_RULE 'DEFINED-ON-LINE '413) 
(PUT 'EXPAND_RULE 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'EXPAND_RULE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPAND_RULE (U)
    (CONS (CAR U)
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X (CDR U))
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (X)
                                (CONS
                                 (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ Y (CAR X))
                                   (COND ((NULL Y) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (Y)
                                                       (EXPAND_TERMINAL Y))
                                                     (CAR Y))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ Y (CDR Y))
                                   (COND ((NULL Y) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (Y) (EXPAND_TERMINAL Y))
                                             (CAR Y))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL))
                                 (CDR X)))
                              (CAR X))
                             NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS
                     ((LAMBDA (X)
                        (CONS
                         (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                           (SETQ Y (CAR X))
                           (COND ((NULL Y) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (Y) (EXPAND_TERMINAL Y))
                                             (CAR Y))
                                            NIL)))
                          LOOPLABEL
                           (SETQ Y (CDR Y))
                           (COND ((NULL Y) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (Y) (EXPAND_TERMINAL Y)) (CAR Y))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))
                         (CDR X)))
                      (CAR X))
                     NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(GLOBAL '(EXPANSION_COUNT)) 
(SETQ EXPANSION_COUNT 0) 
(PUT 'EXPANSION_NAME 'NUMBER-OF-ARGS 0) 
(PUT 'EXPANSION_NAME 'DEFINED-ON-LINE '421) 
(PUT 'EXPANSION_NAME 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'EXPANSION_NAME 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EXPANSION_NAME NIL
    (COMPRESS
     (APPEND (EXPLODE 'LALR_INTERNAL_)
             (EXPLODE (SETQ EXPANSION_COUNT (PLUS EXPANSION_COUNT 1)))))) 
(PUT 'EXPAND_TERMINAL 'NUMBER-OF-ARGS 1) 
(PUT 'EXPAND_TERMINAL 'DEFINED-ON-LINE '425) 
(PUT 'EXPAND_TERMINAL 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'EXPAND_TERMINAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXPAND_TERMINAL (Z)
    (PROG (G1 G2 G3)
      (COND ((ATOM Z) (RETURN Z))
            ((EQCAR Z 'OPT)
             (PROGN
              (SETQ G1 (EXPANSION_NAME))
              (SETQ PENDING_RULES*
                      (CONS (LIST G1 '(NIL) (LIST (CDR Z))) PENDING_RULES*))
              (RETURN G1)))
            ((EQCAR Z 'SEQ)
             (PROGN
              (SETQ G1 (EXPANSION_NAME))
              (SETQ PENDING_RULES*
                      (CONS (LIST G1 (LIST (CDR Z))) PENDING_RULES*))
              (RETURN G1)))
            ((EQCAR Z 'STAR)
             (PROGN
              (SETQ G1 (EXPANSION_NAME))
              (SETQ G2 (EXPANSION_NAME))
              (COND
               ((AND (CDR Z) (NULL (CDDR Z)) (ATOM (CADR Z)))
                (SETQ G2 (CADR Z)))
               (T
                (SETQ PENDING_RULES*
                        (CONS (LIST G2 (LIST (CDR Z))) PENDING_RULES*))))
              (SETQ PENDING_RULES*
                      (CONS (LIST G1 '(NIL) (LIST (LIST G2 G1) '(CONS $1 $2)))
                            PENDING_RULES*))
              (RETURN G1)))
            ((EQCAR Z 'PLUS)
             (PROGN
              (SETQ G1 (EXPANSION_NAME))
              (SETQ G2 (EXPANSION_NAME))
              (COND
               ((AND (CDR Z) (NULL (CDDR Z)) (ATOM (CADR Z)))
                (SETQ G2 (CADR Z)))
               (T
                (SETQ PENDING_RULES*
                        (CONS (LIST G2 (LIST (CDR Z))) PENDING_RULES*))))
              (SETQ PENDING_RULES*
                      (CONS
                       (LIST G1 (LIST (LIST G2) '(LIST $1))
                             (LIST (LIST G2 G1) '(CONS $1 $2)))
                       PENDING_RULES*))
              (RETURN G1)))
            ((AND (EQCAR Z 'LIST) (CDR Z))
             (PROGN
              (SETQ G1 (EXPANSION_NAME))
              (SETQ G2 (EXPANSION_NAME))
              (SETQ G3 (EXPANSION_NAME))
              (COND
               ((AND (CDDR Z) (NULL (CDDDR Z)) (ATOM (CADDR Z)))
                (SETQ G2 (CADDR Z)))
               (T
                (SETQ PENDING_RULES*
                        (CONS (LIST G2 (LIST (CDDR Z))) PENDING_RULES*))))
              (SETQ PENDING_RULES*
                      (CONS
                       (LIST G3 '(NIL)
                             (LIST (LIST (CADR Z) G2 G3) '(CONS $2 $3)))
                       PENDING_RULES*))
              (SETQ PENDING_RULES*
                      (CONS (LIST G1 '(NIL) (LIST (LIST G2 G3) '(CONS $1 $2)))
                            PENDING_RULES*))
              (RETURN G1)))
            ((AND (EQCAR Z 'LISTPLUS) (CDR Z))
             (PROGN
              (SETQ G1 (EXPANSION_NAME))
              (SETQ G2 (EXPANSION_NAME))
              (SETQ G3 (EXPANSION_NAME))
              (COND
               ((AND (CDDR Z) (NULL (CDDDR Z)) (ATOM (CADDR Z)))
                (SETQ G2 (CADDR Z)))
               (T
                (SETQ PENDING_RULES*
                        (CONS (LIST G2 (LIST (CDDR Z))) PENDING_RULES*))))
              (SETQ PENDING_RULES*
                      (CONS
                       (LIST G3 '(NIL)
                             (LIST (LIST (CADR Z) G2 G3) '(CONS $2 $3)))
                       PENDING_RULES*))
              (SETQ PENDING_RULES*
                      (CONS (LIST G1 (LIST (LIST G2 G3) '(CONS $1 $2)))
                            PENDING_RULES*))
              (RETURN G1)))
            ((EQCAR Z 'OR)
             (PROGN
              (SETQ G1 (EXPANSION_NAME))
              (SETQ PENDING_RULES*
                      (CONS
                       (CONS G1
                             (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                               (SETQ Q (CDR Z))
                               (COND ((NULL Q) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (Q) (LIST (LIST Q)))
                                                 (CAR Q))
                                                NIL)))
                              LOOPLABEL
                               (SETQ Q (CDR Q))
                               (COND ((NULL Q) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (Q) (LIST (LIST Q))) (CAR Q))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                       PENDING_RULES*))
              (RETURN G1)))
            (T (REDERR "Invalid item in a rule"))))) 
(PUT 'CARRASSOC 'NUMBER-OF-ARGS 2) 
(PUT 'CARRASSOC 'DEFINED-ON-LINE '512) 
(PUT 'CARRASSOC 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'CARRASSOC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CARRASSOC (KEY ALIST)
    (PROG (W)
      (COND ((NOT (ATOM (SETQ W (RASSOC KEY ALIST)))) (RETURN (CAR W))))
      (TERPRI)
      (PRINC "RASSOC trouble: ")
      (PRIN1 KEY)
      (PRINC " ")
      (PRINT ALIST)
      (REDERR "rassoc trouble"))) 
(PUT 'LALR_SET_GRAMMAR 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_SET_GRAMMAR 'DEFINED-ON-LINE '521) 
(PUT 'LALR_SET_GRAMMAR 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_SET_GRAMMAR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_SET_GRAMMAR (PRECEDENCE_LIST GRAMMAR)
    (PROG (TERMINALS)
      (SETQ GRAMMAR (LALR_AUGMENT_GRAMMAR GRAMMAR))
      (SETQ NONTERMINALS (LALR_COLLECT_NONTERMINALS GRAMMAR))
      (SETQ TERMINALS (LALR_COLLECT_TERMINALS GRAMMAR))
      (SETQ TERMINAL_CODES (LALR_GET_LEX_CODES TERMINALS))
      (LALR_SET_NONTERMINAL_CODES)
      (SETQ PRECEDENCE_TABLE
              (LALR_CREATE_PRECEDENCE_TABLE PRECEDENCE_LIST TERMINAL_CODES))
      (LALR_PROCESS_PRODUCTIONS GRAMMAR TERMINAL_CODES)
      (LALR_PRECALCULATE_FIRST_SETS)
      (SETQ TERMINALS
              (PROG (TERMINAL FORALL-RESULT FORALL-ENDPTR)
                (SETQ TERMINAL TERMINALS)
                (COND ((NULL TERMINAL) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (TERMINAL)
                                    (CARRASSOC (INTERN TERMINAL)
                                     TERMINAL_CODES))
                                  (CAR TERMINAL))
                                 NIL)))
               LOOPLABEL
                (SETQ TERMINAL (CDR TERMINAL))
                (COND ((NULL TERMINAL) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (TERMINAL)
                            (CARRASSOC (INTERN TERMINAL) TERMINAL_CODES))
                          (CAR TERMINAL))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ SYMBOLS (APPEND NONTERMINALS TERMINALS))
      (COND
       (*LALR_VERBOSE
        (PROGN
         (LALR_PRINT_TERMINALS_AND_CODES TERMINALS)
         (LALR_PRINT_NONTERMINALS_AND_PRODUCTIONS)
         (LALR_PRINT_FIRST_INFORMATION)))))) 
(PUT 'LALR_AUGMENT_GRAMMAR 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_AUGMENT_GRAMMAR 'DEFINED-ON-LINE '548) 
(PUT 'LALR_AUGMENT_GRAMMAR 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_AUGMENT_GRAMMAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_AUGMENT_GRAMMAR (GRAMMAR)
    (COND ((ASSOC '|s'| GRAMMAR) GRAMMAR)
          (T (CONS (LIST '|s'| (LIST (LIST (CAAR GRAMMAR)))) GRAMMAR)))) 
(PUT 'LALR_CREATE_PRECEDENCE_TABLE 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_CREATE_PRECEDENCE_TABLE 'DEFINED-ON-LINE '552) 
(PUT 'LALR_CREATE_PRECEDENCE_TABLE 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_CREATE_PRECEDENCE_TABLE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_CREATE_PRECEDENCE_TABLE (PRECEDENCE_LIST LEX_CODES)
    (PROG (TABLE ASSOCIATIVITY NEXT_PRECEDENCE TERMINAL_CODE W)
      (SETQ TABLE (MKVECT (CAAR LEX_CODES)))
      (SETQ NEXT_PRECEDENCE 0)
      (SETQ ASSOCIATIVITY '|:LEFT|)
      (PROG (X)
        (SETQ X PRECEDENCE_LIST)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND
             ((MEMBER X '(|:LEFT| |:NONE| |:RIGHT|)) (SETQ ASSOCIATIVITY X))
             (T
              (PROGN
               (PROG (XX)
                 (SETQ XX (COND ((ATOM X) (LIST X)) (T X)))
                LAB
                 (COND ((NULL XX) (RETURN NIL)))
                 ((LAMBDA (XX)
                    (PROGN
                     (SETQ W (RASSOC (INTERN XX) LEX_CODES))
                     (COND
                      (W
                       (PROGN
                        (SETQ TERMINAL_CODE (CAR W))
                        (PUTV TABLE TERMINAL_CODE
                              (CONS NEXT_PRECEDENCE ASSOCIATIVITY)))))))
                  (CAR XX))
                 (SETQ XX (CDR XX))
                 (GO LAB))
               (SETQ NEXT_PRECEDENCE (PLUS NEXT_PRECEDENCE 1)))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN TABLE))) 
(PUT 'LALR_SET_NONTERMINAL_CODES 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_SET_NONTERMINAL_CODES 'DEFINED-ON-LINE '574) 
(PUT 'LALR_SET_NONTERMINAL_CODES 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_SET_NONTERMINAL_CODES 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_SET_NONTERMINAL_CODES NIL
    (PROG (CODE)
      (SETQ CODE 0)
      (PROG (X)
        (SETQ X NONTERMINALS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (COND ((EQUAL X '|s'|) (PUT X 'LALR_NONTERMINAL_CODE (MINUS 1)))
                  (T
                   (PROGN
                    (PUT X 'LALR_NONTERMINAL_CODE CODE)
                    (COND
                     (*LALR_VERBOSE
                      (SETQ NONTERMINAL_CODES
                              (CONS (CONS CODE X) NONTERMINAL_CODES))))
                    (SETQ CODE (PLUS CODE 1)))))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND
       (*LALR_VERBOSE
        (SETQ NONTERMINAL_CODES
                (CONS (CONS (MINUS 1) '|s'|) NONTERMINAL_CODES)))))) 
(PUT 'LALR_PROCESS_PRODUCTIONS 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_PROCESS_PRODUCTIONS 'DEFINED-ON-LINE '590) 
(PUT 'LALR_PROCESS_PRODUCTIONS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_PROCESS_PRODUCTIONS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_PROCESS_PRODUCTIONS (GRAMMAR LEX_CODES)
    (PROG (X PRODUCTIONS PRODUCTIONS_PROCESSED W RULE SEMANTIC_ACTION)
      (PROG (PRODUCTIONS)
        (SETQ PRODUCTIONS GRAMMAR)
       LAB
        (COND ((NULL PRODUCTIONS) (RETURN NIL)))
        ((LAMBDA (PRODUCTIONS)
           (PROGN
            (SETQ X (CAR PRODUCTIONS))
            (SETQ PRODUCTIONS_PROCESSED NIL)
            (PROG (PRODUCTION)
              (SETQ PRODUCTION (CDR PRODUCTIONS))
             LAB
              (COND ((NULL PRODUCTION) (RETURN NIL)))
              ((LAMBDA (PRODUCTION)
                 (PROGN
                  (SETQ RULE (CAR PRODUCTION))
                  (SETQ SEMANTIC_ACTION (CDR PRODUCTION))
                  (SETQ RULE
                          (PROG (SYMBOL FORALL-RESULT FORALL-ENDPTR)
                            (SETQ SYMBOL RULE)
                            (COND ((NULL SYMBOL) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (SYMBOL)
                                                (COND
                                                 ((MEMBER (INTERN SYMBOL)
                                                          NONTERMINALS)
                                                  (INTERN SYMBOL))
                                                 (T
                                                  (CARRASSOC (INTERN SYMBOL)
                                                   LEX_CODES))))
                                              (CAR SYMBOL))
                                             NIL)))
                           LOOPLABEL
                            (SETQ SYMBOL (CDR SYMBOL))
                            (COND ((NULL SYMBOL) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (SYMBOL)
                                        (COND
                                         ((MEMBER (INTERN SYMBOL) NONTERMINALS)
                                          (INTERN SYMBOL))
                                         (T
                                          (CARRASSOC (INTERN SYMBOL)
                                           LEX_CODES))))
                                      (CAR SYMBOL))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))
                  (SETQ PRODUCTION (CONS RULE SEMANTIC_ACTION))
                  (SETQ PRODUCTIONS_PROCESSED
                          (CONS PRODUCTION PRODUCTIONS_PROCESSED))))
               (CAR PRODUCTION))
              (SETQ PRODUCTION (CDR PRODUCTION))
              (GO LAB))
            (COND
             ((SETQ W (GET (INTERN X) 'LALR_PRODUCES))
              (SETQ PRODUCTIONS_PROCESSED (APPEND W PRODUCTIONS_PROCESSED))))
            (PUT (INTERN X) 'LALR_PRODUCES PRODUCTIONS_PROCESSED)))
         (CAR PRODUCTIONS))
        (SETQ PRODUCTIONS (CDR PRODUCTIONS))
        (GO LAB)))) 
(PUT 'LALR_PRECALCULATE_FIRST_SETS 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_PRECALCULATE_FIRST_SETS 'DEFINED-ON-LINE '611) 
(PUT 'LALR_PRECALCULATE_FIRST_SETS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_PRECALCULATE_FIRST_SETS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_PRECALCULATE_FIRST_SETS NIL
    (PROG (MORE_ADDED X_FIRST_SET RHS W)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ MORE_ADDED NIL)
         (PROG (X)
           (SETQ X NONTERMINALS)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (PROGN
               (SETQ X_FIRST_SET (GET X 'LALR_FIRST))
               (PROG (PRODUCTION)
                 (SETQ PRODUCTION (LALR_PRODUCTIONS X))
                LAB
                 (COND ((NULL PRODUCTION) (RETURN NIL)))
                 ((LAMBDA (PRODUCTION)
                    (PROGN
                     (SETQ RHS (CAR PRODUCTION))
                     (PROG ()
                      WHILELABEL
                       (COND
                        ((NOT
                          (AND RHS (NOT (NUMBERP (CAR RHS)))
                               (MEMBER NIL
                                       (SETQ W (GET (CAR RHS) 'LALR_FIRST)))))
                         (RETURN NIL)))
                       (PROGN
                        (SETQ X_FIRST_SET (UNION (DELETE NIL W) X_FIRST_SET))
                        (SETQ RHS (CDR RHS)))
                       (GO WHILELABEL))
                     (COND
                      ((NULL RHS)
                       (SETQ X_FIRST_SET (UNION '(NIL) X_FIRST_SET)))
                      ((NUMBERP (CAR RHS))
                       (SETQ X_FIRST_SET (UNION (LIST (CAR RHS)) X_FIRST_SET)))
                      (T
                       (SETQ X_FIRST_SET
                               (UNION (GET (CAR RHS) 'LALR_FIRST)
                                      X_FIRST_SET))))))
                  (CAR PRODUCTION))
                 (SETQ PRODUCTION (CDR PRODUCTION))
                 (GO LAB))
               (COND
                ((NEQ X_FIRST_SET (GET X 'LALR_FIRST))
                 (PROGN
                  (SETQ MORE_ADDED T)
                  (PUT X 'LALR_FIRST X_FIRST_SET))))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB)))
        (COND ((NOT (NOT MORE_ADDED)) (GO REPEATLABEL)))))) 
(PUT 'LALR_COLLECT_NONTERMINALS 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_COLLECT_NONTERMINALS 'DEFINED-ON-LINE '637) 
(PUT 'LALR_COLLECT_NONTERMINALS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_COLLECT_NONTERMINALS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_COLLECT_NONTERMINALS (GRAMMAR)
    (LALR_REMOVE_DUPLICATES
     (PROG (PRODUCTIONS FORALL-RESULT FORALL-ENDPTR)
       (SETQ PRODUCTIONS GRAMMAR)
       (COND ((NULL PRODUCTIONS) (RETURN NIL)))
       (SETQ FORALL-RESULT
               (SETQ FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (PRODUCTIONS) (INTERN (CAR PRODUCTIONS)))
                         (CAR PRODUCTIONS))
                        NIL)))
      LOOPLABEL
       (SETQ PRODUCTIONS (CDR PRODUCTIONS))
       (COND ((NULL PRODUCTIONS) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               (CONS
                ((LAMBDA (PRODUCTIONS) (INTERN (CAR PRODUCTIONS)))
                 (CAR PRODUCTIONS))
                NIL))
       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
       (GO LOOPLABEL)))) 
(PUT 'LALR_GET_LEX_CODES 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_GET_LEX_CODES 'DEFINED-ON-LINE '652) 
(PUT 'LALR_GET_LEX_CODES 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_GET_LEX_CODES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_GET_LEX_CODES (TERMINALS)
    (PROG (NONSTANDARD_TERMINALS PREV_LEX_CONTEXT LEX_CODES)
      (PROG (TERMINAL)
        (SETQ TERMINAL TERMINALS)
       LAB
        (COND ((NULL TERMINAL) (RETURN NIL)))
        ((LAMBDA (TERMINAL)
           (COND
            ((STRINGP TERMINAL)
             (SETQ NONSTANDARD_TERMINALS
                     (CONS TERMINAL NONSTANDARD_TERMINALS)))))
         (CAR TERMINAL))
        (SETQ TERMINAL (CDR TERMINAL))
        (GO LAB))
      (SETQ PREV_LEX_CONTEXT (LEX_SAVE_CONTEXT))
      (LEX_CLEANUP)
      (LEX_KEYWORDS NONSTANDARD_TERMINALS)
      (SETQ LEX_CONTEXT (LEX_SAVE_CONTEXT))
      (SETQ LEX_CODES (LEX_EXPORT_CODES))
      (LEX_RESTORE_CONTEXT PREV_LEX_CONTEXT)
      (RETURN LEX_CODES))) 
(PUT 'LALR_GENERATE_LR0_COLLECTION 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_GENERATE_LR0_COLLECTION 'DEFINED-ON-LINE '685) 
(PUT 'LALR_GENERATE_LR0_COLLECTION 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_GENERATE_LR0_COLLECTION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_GENERATE_LR0_COLLECTION NIL
    (PROG (PENDING PREVIOUS_I I_ITEMSET ITEMSET GOTO_ITEMSET GOTO_ITEMSET1
           I_GOTO_ITEMSET)
      (SETQ ITEMSET_COLLECTION (LIST (CONS (LALR_LR0_INITIAL_ITEMSET) 0)))
      (SETQ PENDING (LIST (CAR ITEMSET_COLLECTION)))
      (SETQ PREVIOUS_I 0)
      (PROG ()
       WHILELABEL
        (COND ((NOT PENDING) (RETURN NIL)))
        (PROGN
         (SETQ I_ITEMSET (CAR PENDING))
         (SETQ PENDING (CDR PENDING))
         (SETQ ITEMSET (CAR I_ITEMSET))
         (PROG (X)
           (SETQ X SYMBOLS)
          LAB
           (COND ((NULL X) (RETURN NIL)))
           ((LAMBDA (X)
              (PROGN
               (COND
                ((SETQ GOTO_ITEMSET (LALR_COMPUTE_LR0_GOTO ITEMSET X))
                 (PROGN
                  (COND
                   ((SETQ GOTO_ITEMSET1
                            (ASSOC GOTO_ITEMSET ITEMSET_COLLECTION))
                    (SETQ I_GOTO_ITEMSET GOTO_ITEMSET1))
                   (T
                    (PROGN
                     (SETQ I_GOTO_ITEMSET
                             (CONS GOTO_ITEMSET
                                   (SETQ PREVIOUS_I (PLUS PREVIOUS_I 1))))
                     (SETQ ITEMSET_COLLECTION
                             (CONS I_GOTO_ITEMSET ITEMSET_COLLECTION))
                     (SETQ PENDING (CONS I_GOTO_ITEMSET PENDING)))))
                  (LALR_ADD_GOTO I_ITEMSET X I_GOTO_ITEMSET))))))
            (CAR X))
           (SETQ X (CDR X))
           (GO LAB)))
        (GO WHILELABEL)))) 
(PUT 'LALR_LR0_INITIAL_ITEMSET 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_LR0_INITIAL_ITEMSET 'DEFINED-ON-LINE '717) 
(PUT 'LALR_LR0_INITIAL_ITEMSET 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_LR0_INITIAL_ITEMSET 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_LR0_INITIAL_ITEMSET NIL
    (PROG (START_SYMBOL INITIAL_ITEM)
      (SETQ START_SYMBOL (CAAAR (LALR_PRODUCTIONS '|s'|)))
      (SETQ INITIAL_ITEM (LIST '|s'| '|.| START_SYMBOL))
      (RETURN (LALR_LR0_CLOSURE (LIST INITIAL_ITEM))))) 
(PUT 'LALR_COMPUTE_LR0_GOTO 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_COMPUTE_LR0_GOTO 'DEFINED-ON-LINE '733) 
(PUT 'LALR_COMPUTE_LR0_GOTO 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_COMPUTE_LR0_GOTO 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_COMPUTE_LR0_GOTO (ITEMSET X)
    (PROG (RESULT_KERNEL RESULT_ITEM)
      (PROG (ITEM)
        (SETQ ITEM ITEMSET)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (COND
            ((SETQ RESULT_ITEM (LALR_LR0_MOVE_DOT ITEM X))
             (SETQ RESULT_KERNEL (CONS RESULT_ITEM RESULT_KERNEL)))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (RETURN (LALR_LR0_CLOSURE RESULT_KERNEL)))) 
(PUT 'LALR_LR0_MOVE_DOT 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_LR0_MOVE_DOT 'DEFINED-ON-LINE '747) 
(PUT 'LALR_LR0_MOVE_DOT 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_LR0_MOVE_DOT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_LR0_MOVE_DOT (ITEM X)
    (PROG (R)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (EQUAL (CAR ITEM) '|.|))) (RETURN NIL)))
        (PROGN (SETQ R (CONS (CAR ITEM) R)) (SETQ ITEM (CDR ITEM)))
        (GO WHILELABEL))
      (SETQ ITEM (CDR ITEM))
      (COND ((NOT (AND ITEM (EQUAL (CAR ITEM) X))) (RETURN NIL)))
      (SETQ ITEM (CONS (CAR ITEM) (CONS '|.| (CDR ITEM))))
      (PROG ()
       WHILELABEL
        (COND ((NOT R) (RETURN NIL)))
        (PROGN (SETQ ITEM (CONS (CAR R) ITEM)) (SETQ R (CDR R)))
        (GO WHILELABEL))
      (RETURN ITEM))) 
(PUT 'LALR_LR0_CLOSURE 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_LR0_CLOSURE 'DEFINED-ON-LINE '774) 
(PUT 'LALR_LR0_CLOSURE 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_LR0_CLOSURE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_LR0_CLOSURE (ITEMSET)
    (PROG (ADDED PENDING TAIL X RULE NEW_ITEMS Y)
      (PROG (ITEM)
        (SETQ ITEM ITEMSET)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (SETQ TAIL (CDR (MEMBER '|.| ITEM)))
            (COND
             ((AND TAIL (SETQ X (CAR TAIL)) (IDP X) (NOT (MEMBER X PENDING)))
              (SETQ PENDING (CONS X PENDING))))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT PENDING) (RETURN NIL)))
        (PROGN
         (SETQ X (CAR PENDING))
         (SETQ PENDING (CDR PENDING))
         (SETQ ADDED (CONS X ADDED))
         (PROG (PRODUCTION)
           (SETQ PRODUCTION (LALR_PRODUCTIONS X))
          LAB
           (COND ((NULL PRODUCTION) (RETURN NIL)))
           ((LAMBDA (PRODUCTION)
              (PROGN
               (SETQ RULE (CAR PRODUCTION))
               (SETQ NEW_ITEMS (CONS (CONS X (CONS '|.| RULE)) NEW_ITEMS))
               (COND
                ((AND RULE (SETQ Y (CAR RULE)) (IDP Y)
                      (NOT (OR (MEMBER Y ADDED) (MEMBER Y PENDING))))
                 (SETQ PENDING (CONS Y PENDING))))))
            (CAR PRODUCTION))
           (SETQ PRODUCTION (CDR PRODUCTION))
           (GO LAB)))
        (GO WHILELABEL))
      (RETURN (UNION ITEMSET NEW_ITEMS)))) 
(PUT 'LALR_GENERATE_COLLECTION 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_GENERATE_COLLECTION 'DEFINED-ON-LINE '811) 
(PUT 'LALR_GENERATE_COLLECTION 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_GENERATE_COLLECTION 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_GENERATE_COLLECTION NIL
    (PROG (ITEMSET0 PROPAGATION_LIST)
      (PROG (I_ITEMSET)
        (SETQ I_ITEMSET ITEMSET_COLLECTION)
       LAB
        (COND ((NULL I_ITEMSET) (RETURN NIL)))
        ((LAMBDA (I_ITEMSET)
           (RPLACA I_ITEMSET
                   (LALR_LR0_ITEMSET_TO_LALR_KERNEL (CAR I_ITEMSET))))
         (CAR I_ITEMSET))
        (SETQ I_ITEMSET (CDR I_ITEMSET))
        (GO LAB))
      (SETQ PROPAGATION_LIST (LALR_ANALYZE_LOOKAHEADS))
      (SETQ ITEMSET0 (CARRASSOC 0 ITEMSET_COLLECTION))
      (LALR_ADD_LOOKAHEAD (CAR ITEMSET0) 0)
      (LALR_PROPAGATE_LOOKAHEADS PROPAGATION_LIST)
      (PROG (I_ITEMSET)
        (SETQ I_ITEMSET ITEMSET_COLLECTION)
       LAB
        (COND ((NULL I_ITEMSET) (RETURN NIL)))
        ((LAMBDA (I_ITEMSET) (RPLACA I_ITEMSET (LALR_CLOSURE (CAR I_ITEMSET))))
         (CAR I_ITEMSET))
        (SETQ I_ITEMSET (CDR I_ITEMSET))
        (GO LAB))
      (COND (*LALR_VERBOSE (LALR_PRINT_COLLECTION))))) 
(PUT 'LALR_LR0_ITEMSET_TO_LALR_KERNEL 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_LR0_ITEMSET_TO_LALR_KERNEL 'DEFINED-ON-LINE '853) 
(PUT 'LALR_LR0_ITEMSET_TO_LALR_KERNEL 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_LR0_ITEMSET_TO_LALR_KERNEL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_LR0_ITEMSET_TO_LALR_KERNEL (ITEMSET)
    (PROG (KERNEL)
      (PROG (ITEM)
        (SETQ ITEM ITEMSET)
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (COND
            ((OR (EQUAL (CAR ITEM) '|s'|) (NEQ (CADR ITEM) '|.|))
             (SETQ KERNEL (CONS (LIST ITEM) KERNEL)))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (RETURN KERNEL))) 
(PUT 'LALR_ADD_LOOKAHEAD 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_ADD_LOOKAHEAD 'DEFINED-ON-LINE '865) 
(PUT 'LALR_ADD_LOOKAHEAD 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_ADD_LOOKAHEAD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_ADD_LOOKAHEAD (ITEM LOOKAHEAD)
    (COND (ITEM (RPLACD ITEM (CONS LOOKAHEAD (CDR ITEM))))
          (T (RPLACD ITEM (LIST LOOKAHEAD))))) 
(PUT 'LALR_PROPAGATE_LOOKAHEADS 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_PROPAGATE_LOOKAHEADS 'DEFINED-ON-LINE '875) 
(PUT 'LALR_PROPAGATE_LOOKAHEADS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_PROPAGATE_LOOKAHEADS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_PROPAGATE_LOOKAHEADS (PROPAGATION_LIST)
    (PROG (MORE_PROPAGATED SRC DEST)
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ MORE_PROPAGATED NIL)
         (PROG (ITEM_PAIR)
           (SETQ ITEM_PAIR PROPAGATION_LIST)
          LAB
           (COND ((NULL ITEM_PAIR) (RETURN NIL)))
           ((LAMBDA (ITEM_PAIR)
              (PROGN
               (SETQ SRC (CAR ITEM_PAIR))
               (SETQ DEST (CDR ITEM_PAIR))
               (PROG (LOOKAHEAD)
                 (SETQ LOOKAHEAD (CDR SRC))
                LAB
                 (COND ((NULL LOOKAHEAD) (RETURN NIL)))
                 ((LAMBDA (LOOKAHEAD)
                    (COND
                     ((NOT (MEMBER LOOKAHEAD (CDR DEST)))
                      (PROGN
                       (LALR_ADD_LOOKAHEAD DEST LOOKAHEAD)
                       (SETQ MORE_PROPAGATED T)))))
                  (CAR LOOKAHEAD))
                 (SETQ LOOKAHEAD (CDR LOOKAHEAD))
                 (GO LAB))))
            (CAR ITEM_PAIR))
           (SETQ ITEM_PAIR (CDR ITEM_PAIR))
           (GO LAB)))
        (COND ((NOT (NOT MORE_PROPAGATED)) (GO REPEATLABEL)))))) 
(PUT 'LALR_ANALYZE_LOOKAHEADS 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_ANALYZE_LOOKAHEADS 'DEFINED-ON-LINE '900) 
(PUT 'LALR_ANALYZE_LOOKAHEADS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_ANALYZE_LOOKAHEADS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_ANALYZE_LOOKAHEADS NIL
    (PROG (PROPAGATION_LIST LOOKAHEADS DEST_I_ITEMSET DEST_ITEM DUMMY_ITEM BYXD
           XD X D)
      (PROG (SRC_I_ITEMSET)
        (SETQ SRC_I_ITEMSET ITEMSET_COLLECTION)
       LAB
        (COND ((NULL SRC_I_ITEMSET) (RETURN NIL)))
        ((LAMBDA (SRC_I_ITEMSET)
           (PROG (SRC_ITEM)
             (SETQ SRC_ITEM (CAR SRC_I_ITEMSET))
            LAB
             (COND ((NULL SRC_ITEM) (RETURN NIL)))
             ((LAMBDA (SRC_ITEM)
                (PROGN
                 (SETQ DUMMY_ITEM (CONS (CAR SRC_ITEM) '(-1)))
                 (PROG (DUMMY_CLOSURE_ITEM)
                   (SETQ DUMMY_CLOSURE_ITEM (LALR_CLOSURE (LIST DUMMY_ITEM)))
                  LAB
                   (COND ((NULL DUMMY_CLOSURE_ITEM) (RETURN NIL)))
                   ((LAMBDA (DUMMY_CLOSURE_ITEM)
                      (PROGN
                       (SETQ BYXD (CAR DUMMY_CLOSURE_ITEM))
                       (SETQ LOOKAHEADS (CDR DUMMY_CLOSURE_ITEM))
                       (COND
                        ((SETQ XD (CDR (MEMBER '|.| BYXD)))
                         (PROGN
                          (SETQ X (CAR XD))
                          (SETQ D (CDR XD))
                          (SETQ DEST_I_ITEMSET (LALR_GOTO SRC_I_ITEMSET X))
                          (SETQ DEST_ITEM
                                  (LALR_ITEM_WITH_RULE
                                   (LALR_LR0_MOVE_DOT BYXD X)
                                   (CAR DEST_I_ITEMSET)))
                          (PROG (A)
                            (SETQ A LOOKAHEADS)
                           LAB
                            (COND ((NULL A) (RETURN NIL)))
                            ((LAMBDA (A)
                               (COND
                                ((EQUAL A (MINUS 1))
                                 (SETQ PROPAGATION_LIST
                                         (CONS (CONS SRC_ITEM DEST_ITEM)
                                               PROPAGATION_LIST)))
                                (T (LALR_ADD_LOOKAHEAD DEST_ITEM A))))
                             (CAR A))
                            (SETQ A (CDR A))
                            (GO LAB)))))))
                    (CAR DUMMY_CLOSURE_ITEM))
                   (SETQ DUMMY_CLOSURE_ITEM (CDR DUMMY_CLOSURE_ITEM))
                   (GO LAB))))
              (CAR SRC_ITEM))
             (SETQ SRC_ITEM (CDR SRC_ITEM))
             (GO LAB)))
         (CAR SRC_I_ITEMSET))
        (SETQ SRC_I_ITEMSET (CDR SRC_I_ITEMSET))
        (GO LAB))
      (RETURN PROPAGATION_LIST))) 
(PUT 'LALR_ITEM_WITH_RULE 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_ITEM_WITH_RULE 'DEFINED-ON-LINE '933) 
(PUT 'LALR_ITEM_WITH_RULE 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_ITEM_WITH_RULE 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_ITEM_WITH_RULE (RULE ITEMSET) (ASSOC RULE ITEMSET)) 
(PUT 'LALR_CLOSURE 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_CLOSURE 'DEFINED-ON-LINE '952) 
(PUT 'LALR_CLOSURE 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_CLOSURE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_CLOSURE (ITEMSET)
    (PROG (PENDING ITEM TAIL X GEN_LOOKAHEADS GEN_RULE GEN_ITEM)
      (SETQ PENDING ITEMSET)
      (PROG ()
       WHILELABEL
        (COND ((NOT PENDING) (RETURN NIL)))
        (PROGN
         (SETQ ITEM (CAR PENDING))
         (SETQ PENDING (CDR PENDING))
         (PROG (LOOKAHEAD)
           (SETQ LOOKAHEAD (CDR ITEM))
          LAB
           (COND ((NULL LOOKAHEAD) (RETURN NIL)))
           ((LAMBDA (LOOKAHEAD)
              (PROGN
               (SETQ TAIL (CDR (MEMBER '|.| (CAR ITEM))))
               (COND
                ((AND TAIL (SETQ X (CAR TAIL)) (IDP X))
                 (PROG (PRODUCTION)
                   (SETQ PRODUCTION (LALR_PRODUCTIONS X))
                  LAB
                   (COND ((NULL PRODUCTION) (RETURN NIL)))
                   ((LAMBDA (PRODUCTION)
                      (PROGN
                       (SETQ GEN_LOOKAHEADS
                               (LALR_FIRST
                                (APPEND (CDR TAIL) (LIST LOOKAHEAD))))
                       (SETQ GEN_RULE (CONS X (CONS '|.| (CAR PRODUCTION))))
                       (SETQ GEN_ITEM (LALR_ITEM_WITH_RULE GEN_RULE ITEMSET))
                       (COND
                        (GEN_ITEM
                         (SETQ GEN_LOOKAHEADS
                                 (SETDIFF GEN_LOOKAHEADS (CDR GEN_ITEM))))
                        (T
                         (PROGN
                          (SETQ GEN_ITEM (CONS GEN_RULE NIL))
                          (SETQ ITEMSET (CONS GEN_ITEM ITEMSET)))))
                       (COND
                        (GEN_LOOKAHEADS
                         (PROGN
                          (SETQ PENDING
                                  (CONS (CONS GEN_RULE GEN_LOOKAHEADS)
                                        PENDING))
                          (PROG (GEN_LOOKAHEAD)
                            (SETQ GEN_LOOKAHEAD GEN_LOOKAHEADS)
                           LAB
                            (COND ((NULL GEN_LOOKAHEAD) (RETURN NIL)))
                            ((LAMBDA (GEN_LOOKAHEAD)
                               (LALR_ADD_LOOKAHEAD GEN_ITEM GEN_LOOKAHEAD))
                             (CAR GEN_LOOKAHEAD))
                            (SETQ GEN_LOOKAHEAD (CDR GEN_LOOKAHEAD))
                            (GO LAB)))))))
                    (CAR PRODUCTION))
                   (SETQ PRODUCTION (CDR PRODUCTION))
                   (GO LAB))))))
            (CAR LOOKAHEAD))
           (SETQ LOOKAHEAD (CDR LOOKAHEAD))
           (GO LAB)))
        (GO WHILELABEL))
      (RETURN ITEMSET))) 
(PUT 'LALR_FIRST 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_FIRST 'DEFINED-ON-LINE '983) 
(PUT 'LALR_FIRST 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_FIRST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_FIRST (STRING)
    (PROG (RESULTS W)
      (PROG ()
       WHILELABEL
        (COND
         ((NOT
           (AND STRING (NOT (NUMBERP (CAR STRING)))
                (MEMBER NIL (SETQ W (GET (CAR STRING) 'LALR_FIRST)))))
          (RETURN NIL)))
        (PROGN
         (SETQ RESULTS (UNION (DELETE NIL W) RESULTS))
         (SETQ STRING (CDR STRING)))
        (GO WHILELABEL))
      (COND ((NULL STRING) (SETQ RESULTS (CONS NIL RESULTS)))
            ((NUMBERP (CAR STRING))
             (SETQ RESULTS (UNION (LIST (CAR STRING)) RESULTS)))
            (T (SETQ RESULTS (UNION W RESULTS))))
      (RETURN RESULTS))) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_TABLE 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_TABLE 'DEFINED-ON-LINE '1011) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_TABLE 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_TABLE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_MAKE_COMPRESSED_ACTION_TABLE NIL
    (PROG (TABLE)
      (SETQ TABLE (MKVECT (SUB1 (LENGTH ITEMSET_COLLECTION))))
      (PROG (I_ITEMSET)
        (SETQ I_ITEMSET ITEMSET_COLLECTION)
       LAB
        (COND ((NULL I_ITEMSET) (RETURN NIL)))
        ((LAMBDA (I_ITEMSET)
           (PUTV TABLE (CDR I_ITEMSET)
                 (LALR_MAKE_COMPRESSED_ACTION_ROW I_ITEMSET)))
         (CAR I_ITEMSET))
        (SETQ I_ITEMSET (CDR I_ITEMSET))
        (GO LAB))
      (COND (*LALR_VERBOSE (LALR_PRINT_COMPRESSED_ACTION_TABLE TABLE)))
      (RETURN TABLE))) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_ROW 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_ROW 'DEFINED-ON-LINE '1022) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_ROW 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_ROW 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_MAKE_COMPRESSED_ACTION_ROW (I_ITEMSET)
    (PROG (ACTION_LIST)
      (SETQ ACTION_LIST (LALR_LIST_OF_ACTIONS I_ITEMSET))
      (SETQ ACTION_LIST (LALR_RESOLVE_CONFLICTS ACTION_LIST (CDR I_ITEMSET)))
      (RETURN (LALR_MAKE_COMPRESSED_ACTION_ROW1 ACTION_LIST)))) 
(PUT 'ORDERACTIONS 'NUMBER-OF-ARGS 2) 
(PUT 'ORDERACTIONS 'DEFINED-ON-LINE '1033) 
(PUT 'ORDERACTIONS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'ORDERACTIONS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ORDERACTIONS (A B)
    (COND ((LESSP (CAR A) (CAR B)) T) ((GREATERP (CAR A) (CAR B)) NIL)
          ((EQUAL (CAADR A) (CAADR B)) (ORDP (CDR A) (CDR B)))
          ((EQUAL (CAADR A) 'SHIFT) T) (T NIL))) 
(PUT 'LALR_RESOLVE_CONFLICTS 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_RESOLVE_CONFLICTS 'DEFINED-ON-LINE '1040) 
(PUT 'LALR_RESOLVE_CONFLICTS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_RESOLVE_CONFLICTS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_RESOLVE_CONFLICTS (ACTION_LIST ITEMSET_I)
    (PROG (CONFLICTING_ACTIONS RESULTS SHIFT REDUCE CHOSEN_ACTION SHIFT_OP
           REDUCE_OP ASSOCIATIVITY SHIFT_PRECEDENCE REDUCE_PRECEDENCE)
      (SETQ ACTION_LIST (SORT ACTION_LIST (FUNCTION ORDERACTIONS)))
      (PROG ()
       WHILELABEL
        (COND ((NOT ACTION_LIST) (RETURN NIL)))
        (PROGN
         (SETQ CONFLICTING_ACTIONS (LIST (CAR ACTION_LIST)))
         (SETQ ACTION_LIST (CDR ACTION_LIST))
         (PROG ()
          WHILELABEL
           (COND
            ((NOT
              (AND ACTION_LIST
                   (EQUAL (CAAR ACTION_LIST) (CAAR CONFLICTING_ACTIONS))))
             (RETURN NIL)))
           (PROGN
            (SETQ CONFLICTING_ACTIONS
                    (CONS (CAR ACTION_LIST) CONFLICTING_ACTIONS))
            (SETQ ACTION_LIST (CDR ACTION_LIST)))
           (GO WHILELABEL))
         (SETQ SHIFT (SETQ REDUCE (SETQ CHOSEN_ACTION NIL)))
         (COND
          ((NULL (CDR CONFLICTING_ACTIONS))
           (SETQ CHOSEN_ACTION (CAR CONFLICTING_ACTIONS)))
          (T
           (PROG (ACTION)
             (SETQ ACTION CONFLICTING_ACTIONS)
            LAB
             (COND ((NULL ACTION) (RETURN NIL)))
             ((LAMBDA (ACTION)
                (PROGN
                 (COND ((EQUAL (CAADR ACTION) 'SHIFT) (SETQ SHIFT ACTION))
                       ((NULL REDUCE) (SETQ REDUCE ACTION))
                       (T
                        (LALR_WARN_REDUCE_REDUCE_CONFLICT REDUCE ACTION
                         ITEMSET_I)))
                 (COND
                  ((AND SHIFT REDUCE)
                   (PROGN
                    (SETQ SHIFT_OP (CAR SHIFT))
                    (PROG (SYMBOL)
                      (SETQ SYMBOL (CADR (CADR REDUCE)))
                     LAB
                      (COND ((NULL SYMBOL) (RETURN NIL)))
                      ((LAMBDA (SYMBOL)
                         (COND ((NUMBERP SYMBOL) (SETQ REDUCE_OP SYMBOL))))
                       (CAR SYMBOL))
                      (SETQ SYMBOL (CDR SYMBOL))
                      (GO LAB))
                    (COND
                     ((AND SHIFT_OP REDUCE_OP)
                      (PROGN
                       (SETQ SHIFT_PRECEDENCE (LALR_PRECEDENCE SHIFT_OP))
                       (SETQ REDUCE_PRECEDENCE (LALR_PRECEDENCE REDUCE_OP))
                       (COND
                        ((AND SHIFT_PRECEDENCE REDUCE_PRECEDENCE)
                         (COND
                          ((EQUAL SHIFT_PRECEDENCE REDUCE_PRECEDENCE)
                           (PROGN
                            (SETQ ASSOCIATIVITY (LALR_ASSOCIATIVITY SHIFT_OP))
                            (COND
                             ((EQUAL ASSOCIATIVITY '|:LEFT|) (SETQ SHIFT NIL))
                             ((EQUAL ASSOCIATIVITY '|:RIGHT|)
                              (SETQ REDUCE NIL))
                             (T (SETQ SHIFT (SETQ REDUCE NIL))))))
                          ((LESSP SHIFT_PRECEDENCE REDUCE_PRECEDENCE)
                           (SETQ REDUCE NIL))
                          (T (SETQ SHIFT NIL)))))))))))))
              (CAR ACTION))
             (SETQ ACTION (CDR ACTION))
             (GO LAB))))
         (COND
          ((AND SHIFT REDUCE)
           (LALR_WARN_SHIFT_REDUCE_CONFLICT SHIFT REDUCE ITEMSET_I)))
         (SETQ CHOSEN_ACTION (OR CHOSEN_ACTION SHIFT REDUCE))
         (COND (CHOSEN_ACTION (SETQ RESULTS (CONS CHOSEN_ACTION RESULTS)))))
        (GO WHILELABEL))
      (RETURN RESULTS))) 
(PUT 'LALR_LIST_OF_ACTIONS 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_LIST_OF_ACTIONS 'DEFINED-ON-LINE '1096) 
(PUT 'LALR_LIST_OF_ACTIONS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_LIST_OF_ACTIONS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_LIST_OF_ACTIONS (I_ITEMSET)
    (PROG (ACTIONS LHS TERMINAL TAIL RULE REDUCTION_I GOTO_I)
      (PROG (ITEM)
        (SETQ ITEM (CAR I_ITEMSET))
       LAB
        (COND ((NULL ITEM) (RETURN NIL)))
        ((LAMBDA (ITEM)
           (PROGN
            (SETQ LHS (CAAR ITEM))
            (SETQ TAIL (CDR (MEMBER '|.| (CAR ITEM))))
            (COND
             ((AND TAIL (NUMBERP (CAR TAIL)))
              (PROGN
               (SETQ TERMINAL (CAR TAIL))
               (SETQ GOTO_I (CDR (LALR_GOTO I_ITEMSET TERMINAL)))
               (SETQ ACTIONS
                       (CONS (LIST TERMINAL (LIST 'SHIFT GOTO_I)) ACTIONS))))
             ((AND (NULL TAIL) (NEQ LHS '|s'|))
              (PROGN
               (SETQ RULE (DELETE '|.| (CAR ITEM)))
               (SETQ REDUCTION_I (LALR_REDUCTION_INDEX RULE))
               (PROG (LOOKAHEAD)
                 (SETQ LOOKAHEAD (CDR ITEM))
                LAB
                 (COND ((NULL LOOKAHEAD) (RETURN NIL)))
                 ((LAMBDA (LOOKAHEAD)
                    (SETQ ACTIONS
                            (CONS
                             (LIST LOOKAHEAD (LIST 'REDUCE RULE REDUCTION_I))
                             ACTIONS)))
                  (CAR LOOKAHEAD))
                 (SETQ LOOKAHEAD (CDR LOOKAHEAD))
                 (GO LAB))))
             ((AND (NULL TAIL) (EQUAL LHS '|s'|))
              (SETQ ACTIONS (CONS (LIST 0 '(ACCEPT)) ACTIONS))))))
         (CAR ITEM))
        (SETQ ITEM (CDR ITEM))
        (GO LAB))
      (RETURN (LALR_REMOVE_DUPLICATES ACTIONS)))) 
(PUT 'LALR_REMOVE_DUPLICATES 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_REMOVE_DUPLICATES 'DEFINED-ON-LINE '1117) 
(PUT 'LALR_REMOVE_DUPLICATES 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_REMOVE_DUPLICATES 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_REMOVE_DUPLICATES (X)
    (PROG (R)
      (PROG (A)
        (SETQ A X)
       LAB
        (COND ((NULL A) (RETURN NIL)))
        ((LAMBDA (A) (COND ((NOT (MEMBER A R)) (SETQ R (CONS A R))))) (CAR A))
        (SETQ A (CDR A))
        (GO LAB))
      (RETURN (REVERSIP R)))) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_ROW1 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_ROW1 'DEFINED-ON-LINE '1125) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_ROW1 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_MAKE_COMPRESSED_ACTION_ROW1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_MAKE_COMPRESSED_ACTION_ROW1 (ACTION_LIST)
    (PROG (MOST_COMMON_REDUCTION ROW TERMINAL ACTION_TYPE)
      (SETQ MOST_COMMON_REDUCTION (LALR_MOST_COMMON_REDUCTION ACTION_LIST))
      (PROG (ACTION)
        (SETQ ACTION ACTION_LIST)
       LAB
        (COND ((NULL ACTION) (RETURN NIL)))
        ((LAMBDA (ACTION)
           (COND
            ((NEQ (CADR ACTION) MOST_COMMON_REDUCTION)
             (PROGN
              (SETQ TERMINAL (CAR ACTION))
              (SETQ ACTION_TYPE (CAADR ACTION))
              (COND
               ((EQUAL ACTION_TYPE 'SHIFT)
                (SETQ ROW (CONS (CONS TERMINAL (CADADR ACTION)) ROW)))
               ((EQUAL ACTION_TYPE 'ACCEPT)
                (SETQ ROW (CONS (CONS TERMINAL 0) ROW)))
               ((EQUAL ACTION_TYPE 'REDUCE)
                (SETQ ROW
                        (CONS (CONS TERMINAL (MINUS (CAR (CDDADR ACTION))))
                              ROW))))))))
         (CAR ACTION))
        (SETQ ACTION (CDR ACTION))
        (GO LAB))
      (COND
       (MOST_COMMON_REDUCTION
        (SETQ MOST_COMMON_REDUCTION
                (COND ((EQUAL (CAR MOST_COMMON_REDUCTION) 'ACCEPT) 0)
                      (T (MINUS (CADDR MOST_COMMON_REDUCTION)))))))
      (RETURN (CONS ROW MOST_COMMON_REDUCTION)))) 
(PUT 'LALR_MOST_COMMON_REDUCTION 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_MOST_COMMON_REDUCTION 'DEFINED-ON-LINE '1146) 
(PUT 'LALR_MOST_COMMON_REDUCTION 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_MOST_COMMON_REDUCTION 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_MOST_COMMON_REDUCTION (ACTION_LIST)
    (PROG (REDUCTION_COUNT REDUCTION W)
      (PROG (ACTION)
        (SETQ ACTION ACTION_LIST)
       LAB
        (COND ((NULL ACTION) (RETURN NIL)))
        ((LAMBDA (ACTION)
           (COND
            ((OR (EQUAL (CAADR ACTION) 'REDUCE) (EQUAL (CAADR ACTION) 'ACCEPT))
             (PROGN
              (SETQ REDUCTION (CADR ACTION))
              (COND
               ((SETQ W (ASSOC ACTION REDUCTION)) (RPLACD W (PLUS (CDR W) 1)))
               (T
                (SETQ REDUCTION_COUNT
                        (CONS (CONS REDUCTION 1) REDUCTION_COUNT))))))))
         (CAR ACTION))
        (SETQ ACTION (CDR ACTION))
        (GO LAB))
      (COND
       (REDUCTION_COUNT
        (PROGN
         (SETQ W (CAR REDUCTION_COUNT))
         (PROG (ENTRY)
           (SETQ ENTRY (CDR REDUCTION_COUNT))
          LAB
           (COND ((NULL ENTRY) (RETURN NIL)))
           ((LAMBDA (ENTRY)
              (COND ((GREATERP (CDR ENTRY) (CDR W)) (SETQ W ENTRY))))
            (CAR ENTRY))
           (SETQ ENTRY (CDR ENTRY))
           (GO LAB))
         (RETURN (CAR W))))
       (T (RETURN NIL))))) 
(PUT 'LALR_MAKE_COMPRESSED_GOTO_TABLE 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_MAKE_COMPRESSED_GOTO_TABLE 'DEFINED-ON-LINE '1174) 
(PUT 'LALR_MAKE_COMPRESSED_GOTO_TABLE 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_MAKE_COMPRESSED_GOTO_TABLE 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_MAKE_COMPRESSED_GOTO_TABLE NIL
    (PROG (TABLE COLUMN)
      (SETQ TABLE (MKVECT (SUB1 (LENGTH NONTERMINALS))))
      (PROG (X)
        (SETQ X NONTERMINALS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((NEQ X '|s'|)
             (PROGN
              (SETQ COLUMN (LALR_MAKE_COMPRESSED_GOTO_COLUMN X))
              (PUTV TABLE (GET X 'LALR_NONTERMINAL_CODE) COLUMN)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (COND (*LALR_VERBOSE (LALR_PRINT_COMPRESSED_GOTO_TABLE TABLE)))
      (RETURN TABLE))) 
(PUT 'LALR_MAKE_COMPRESSED_GOTO_COLUMN 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_MAKE_COMPRESSED_GOTO_COLUMN 'DEFINED-ON-LINE '1187) 
(PUT 'LALR_MAKE_COMPRESSED_GOTO_COLUMN 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_MAKE_COMPRESSED_GOTO_COLUMN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_MAKE_COMPRESSED_GOTO_COLUMN (X)
    (PROG (GOTO_LIST COLUMN MOST_COMMON_DEST)
      (SETQ GOTO_LIST
              (PROG (ENTRY FORALL-RESULT FORALL-ENDPTR)
                (SETQ ENTRY (GETHASH X GOTO_TABLE))
                (COND ((NULL ENTRY) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ENTRY)
                                    (CONS (CDAR ENTRY) (CDDR ENTRY)))
                                  (CAR ENTRY))
                                 NIL)))
               LOOPLABEL
                (SETQ ENTRY (CDR ENTRY))
                (COND ((NULL ENTRY) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ENTRY) (CONS (CDAR ENTRY) (CDDR ENTRY)))
                          (CAR ENTRY))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ MOST_COMMON_DEST (LALR_MOST_COMMON_DEST GOTO_LIST))
      (PROG (ENTRY)
        (SETQ ENTRY GOTO_LIST)
       LAB
        (COND ((NULL ENTRY) (RETURN NIL)))
        ((LAMBDA (ENTRY)
           (COND
            ((NEQ (CDR ENTRY) MOST_COMMON_DEST)
             (SETQ COLUMN (CONS ENTRY COLUMN)))))
         (CAR ENTRY))
        (SETQ ENTRY (CDR ENTRY))
        (GO LAB))
      (RETURN (CONS COLUMN MOST_COMMON_DEST)))) 
(PUT 'LALR_MOST_COMMON_DEST 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_MOST_COMMON_DEST 'DEFINED-ON-LINE '1199) 
(PUT 'LALR_MOST_COMMON_DEST 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_MOST_COMMON_DEST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_MOST_COMMON_DEST (GOTO_LIST)
    (PROG (DEST_COUNT W)
      (PROG (ENTRY)
        (SETQ ENTRY GOTO_LIST)
       LAB
        (COND ((NULL ENTRY) (RETURN NIL)))
        ((LAMBDA (ENTRY)
           (COND
            ((SETQ W (ASSOC (CDR ENTRY) DEST_COUNT))
             (RPLACD W (PLUS (CDR W) 1)))
            (T (SETQ DEST_COUNT (CONS (CONS (CDR ENTRY) 1) DEST_COUNT)))))
         (CAR ENTRY))
        (SETQ ENTRY (CDR ENTRY))
        (GO LAB))
      (SETQ W (CAR DEST_COUNT))
      (PROG (ENTRY)
        (SETQ ENTRY (CDR DEST_COUNT))
       LAB
        (COND ((NULL ENTRY) (RETURN NIL)))
        ((LAMBDA (ENTRY)
           (COND ((GREATERP (CDR ENTRY) (CDR W)) (SETQ W ENTRY))))
         (CAR ENTRY))
        (SETQ ENTRY (CDR ENTRY))
        (GO LAB))
      (RETURN (CAR W)))) 
(PUT 'LALR_PROCESS_REDUCTIONS 'NUMBER-OF-ARGS 0) 
(PUT 'LALR_PROCESS_REDUCTIONS 'DEFINED-ON-LINE '1215) 
(PUT 'LALR_PROCESS_REDUCTIONS 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_PROCESS_REDUCTIONS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE LALR_PROCESS_REDUCTIONS NIL
    (PROG (REDUCTION_CODES CODE N_REDUCTIONS CANONICAL_REDUCTION
           CODED_CANONICAL_REDUCTION RHS_LENGTH LHS FN REDUCTION_FN
           REDUCTION_LHS REDUCTION_RHS_LENGTH)
      (SETQ CODE (MINUS 1))
      (PROG (X)
        (SETQ X NONTERMINALS)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROG (PRODUCTION)
             (SETQ PRODUCTION (LALR_PRODUCTIONS X))
            LAB
             (COND ((NULL PRODUCTION) (RETURN NIL)))
             ((LAMBDA (PRODUCTION)
                (PROGN
                 (SETQ CANONICAL_REDUCTION
                         (CONS (CONS X (LENGTH (CAR PRODUCTION)))
                               (CDR PRODUCTION)))
                 (SETQ CODED_CANONICAL_REDUCTION
                         (ASSOC CANONICAL_REDUCTION REDUCTION_CODES))
                 (COND
                  ((NULL CODED_CANONICAL_REDUCTION)
                   (PROGN
                    (SETQ CODED_CANONICAL_REDUCTION
                            (CONS CANONICAL_REDUCTION
                                  (SETQ CODE (PLUS CODE 1))))
                    (SETQ REDUCTION_CODES
                            (CONS CODED_CANONICAL_REDUCTION
                                  REDUCTION_CODES)))))
                 (RPLACD PRODUCTION (CDR CODED_CANONICAL_REDUCTION))))
              (CAR PRODUCTION))
             (SETQ PRODUCTION (CDR PRODUCTION))
             (GO LAB)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ N_REDUCTIONS (PLUS CODE 1))
      (SETQ REDUCTION_FN (MKVECT (SUB1 N_REDUCTIONS)))
      (SETQ REDUCTION_LHS (MKVECT16 (SUB1 N_REDUCTIONS)))
      (SETQ REDUCTION_RHS_LENGTH (MKVECT8 (SUB1 N_REDUCTIONS)))
      (PROG (CODED_CANONICAL_REDUCTION)
        (SETQ CODED_CANONICAL_REDUCTION REDUCTION_CODES)
       LAB
        (COND ((NULL CODED_CANONICAL_REDUCTION) (RETURN NIL)))
        ((LAMBDA (CODED_CANONICAL_REDUCTION)
           (PROGN
            (SETQ CODE (CDR CODED_CANONICAL_REDUCTION))
            (SETQ RHS_LENGTH (CDAAR CODED_CANONICAL_REDUCTION))
            (SETQ LHS
                    (GET (CAAAR CODED_CANONICAL_REDUCTION)
                         'LALR_NONTERMINAL_CODE))
            (PUTV16 REDUCTION_LHS CODE LHS)
            (PUTV8 REDUCTION_RHS_LENGTH CODE RHS_LENGTH)
            (COND
             ((CDAR CODED_CANONICAL_REDUCTION)
              (PROGN
               (SETQ FN (CDAR CODED_CANONICAL_REDUCTION))
               (SETQ FN (LALR_CONSTRUCT_FN FN RHS_LENGTH))))
             (T (SETQ FN NIL)))
            (PUTV REDUCTION_FN CODE FN)))
         (CAR CODED_CANONICAL_REDUCTION))
        (SETQ CODED_CANONICAL_REDUCTION (CDR CODED_CANONICAL_REDUCTION))
        (GO LAB))
      (RETURN (LIST REDUCTION_FN REDUCTION_RHS_LENGTH REDUCTION_LHS)))) 
(PUT 'CDRASSOC 'NUMBER-OF-ARGS 2) 
(PUT 'CDRASSOC 'DEFINED-ON-LINE '1253) 
(PUT 'CDRASSOC 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'CDRASSOC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CDRASSOC (KEY ALIST)
    (PROG (W)
      (COND ((NOT (ATOM (SETQ W (ASSOC KEY ALIST)))) (RETURN (CDR W))))
      (TERPRI)
      (PRINC "ASSOC trouble: ")
      (PRIN1 KEY)
      (PRINC " ")
      (PRINT ALIST)
      (REDERR "assoc trouble"))) 
(PUT 'LALR_REDUCTION_INDEX 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_REDUCTION_INDEX 'DEFINED-ON-LINE '1264) 
(PUT 'LALR_REDUCTION_INDEX 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_REDUCTION_INDEX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_REDUCTION_INDEX (RULE)
    (CDRASSOC (CDR RULE) (LALR_PRODUCTIONS (CAR RULE)))) 
(PUT 'LALR_CONSTRUCT_FN 'NUMBER-OF-ARGS 2) 
(PUT 'LALR_CONSTRUCT_FN 'DEFINED-ON-LINE '1268) 
(PUT 'LALR_CONSTRUCT_FN 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_CONSTRUCT_FN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LALR_CONSTRUCT_FN (LAMBDA_EXPR ARGS_N)
    (PROG (FN)
      (SETQ FN (GENSYM))
      (SETQ LAMBDA_EXPR
              (CONS 'LAMBDA (CONS (LALR_MAKE_ARGLIST ARGS_N) LAMBDA_EXPR)))
      ((LAMBDA (*PWRDS) (PUTD FN 'EXPR LAMBDA_EXPR)) NIL)
      (RETURN FN))) 
(PUT 'LALR_MAKE_ARGLIST 'NUMBER-OF-ARGS 1) 
(PUT 'LALR_MAKE_ARGLIST 'DEFINED-ON-LINE '1277) 
(PUT 'LALR_MAKE_ARGLIST 'DEFINED-IN-FILE 'LALR/GENPARSER.RED) 
(PUT 'LALR_MAKE_ARGLIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LALR_MAKE_ARGLIST (N)
    (PROG (I FORALL-RESULT FORALL-ENDPTR)
      (SETQ I 1)
      (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS (INTERN (LIST2STRING (CONS '$ (EXPLODE2 I))))
                            NIL)))
     LOOPLABEL
      (SETQ I (PLUS2 I 1))
      (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS (INTERN (LIST2STRING (CONS '$ (EXPLODE2 I)))) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 