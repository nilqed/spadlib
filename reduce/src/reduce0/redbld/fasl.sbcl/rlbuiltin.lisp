(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RLBUILTIN)) 
(REVISION 'RLBUILTIN
          "$Id: rlbuiltin.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'RLBUILTIN "(c) 2020 T. Sturm") 
(FLUID '(MODE*)) 
(GLOBAL '(RL_BUILTINS*)) 
(PUT 'RL_BUILTIN 'STAT 'RL_BUILTINSTAT) 
(PUT 'RL_BUILTINSTAT 'NUMBER-OF-ARGS 0) 
(DE RL_BUILTINSTAT NIL
    (PROG (L KEY ENTRY)
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* '*LCBKT*)
         (REDERR
          (LIST "expecting '{' in, " "rl_builtin" "but found" CURSYM*)))))
      (SCAN)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ CURSYM* '*RCBKT*)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (IDP CURSYM*))
           (REDERR
            (LIST "expecting identifier in rl_builtin but found" CURSYM*))))
         (SETQ KEY CURSYM*)
         (PROGN
          (SCAN)
          (COND
           ((NEQ CURSYM* 'EQUAL)
            (REDERR
             (LIST "expecting '=' in" "rl_builtin" "but found" CURSYM*)))))
         (COND ((EQ KEY 'DOC) (SETQ ENTRY (RL_BUILTINSTATDOC)))
               ((MEMQ KEY '(NAME))
                (PROGN
                 (SCAN)
                 (COND
                  ((NOT (ATOM CURSYM*))
                   (REDERR
                    (LIST "expecting atom in rl_builtin but found" CURSYM*))))
                 (SETQ ENTRY CURSYM*)
                 (SCAN)))
               (T (REDERR (LIST "unknown keyword" KEY "in rl_builtin"))))
         (PROG (W1)
           (SETQ W1 (CONS KEY ENTRY))
           (SETQ L (CONS W1 L))
           (RETURN W1))
         (COND
          ((NEQ CURSYM* '*RCBKT*)
           (PROGN
            (COND
             ((NEQ CURSYM* '*COMMA*)
              (REDERR
               (LIST "expecting ',' or '}' in rl_builtin but found" CURSYM*))))
            (SCAN)))))
        (GO WHILELABEL))
      (SCAN)
      (RETURN (LIST 'RL_BUILTIN (REVERSIP L))))) 
(PUT 'RL_BUILTINSTATDOC 'NUMBER-OF-ARGS 0) 
(DE RL_BUILTINSTATDOC NIL
    (PROG (KEY ENTRY L)
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* '*LCBKT*)
         (REDERR
          (LIST "expecting '{' in, " "rl_builtin doc" "but found" CURSYM*)))))
      (SCAN)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ CURSYM* '*RCBKT*)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (IDP CURSYM*))
           (REDERR
            (LIST "expecting identifier in rl_builtin doc but found"
                  CURSYM*))))
         (SETQ KEY CURSYM*)
         (COND
          ((MEMQ KEY '(DESCRIPTION RETURNS))
           (PROGN
            (PROGN
             (SCAN)
             (COND
              ((NEQ CURSYM* 'EQUAL)
               (REDERR
                (LIST "expecting '=' in" "rl_builtin" "but found" CURSYM*)))))
            (SCAN)
            (COND
             ((NOT (STRINGP CURSYM*))
              (REDERR
               (LIST "expecting a string in rl_builtin doc but found"
                     CURSYM*))))
            (SETQ ENTRY CURSYM*)
            (SCAN)))
          ((MEMQ KEY '(SYNOPSIS ARG))
           (PROGN
            (PROGN
             (SCAN)
             (COND
              ((NEQ CURSYM* 'EQUAL)
               (REDERR
                (LIST "expecting '=' in" "rl_builtin doc arguments" "but found"
                      CURSYM*)))))
            (PROGN
             (SCAN)
             (COND
              ((NEQ CURSYM* '*LCBKT*)
               (REDERR
                (LIST "expecting '{' in, " "rl_builtin doc arguments"
                      "but found" CURSYM*)))))
            (SETQ ENTRY (RL_SERVICESTATLIST))
            (SCAN)))
          ((MEMQ KEY '(SEEALSO))
           (PROGN
            (PROGN
             (SCAN)
             (COND
              ((NEQ CURSYM* 'EQUAL)
               (REDERR
                (LIST "expecting '=' in" "rl_builtin" "but found" CURSYM*)))))
            (SCAN)
            (COND
             ((NOT (IDP CURSYM*))
              (REDERR
               (LIST "expecting an identifier in rl_builtin doc but found"
                     CURSYM*))))
            (SETQ ENTRY CURSYM*)
            (SCAN)))
          (T (REDERR (LIST "unknown keyword" KEY "in rl_builtin doc"))))
         (PROG (W1)
           (SETQ W1 (CONS KEY ENTRY))
           (SETQ L (CONS W1 L))
           (RETURN W1))
         (COND
          ((NEQ CURSYM* '*RCBKT*)
           (PROGN
            (COND
             ((NEQ CURSYM* '*COMMA*)
              (REDERR
               (LIST "expecting ',' or '}' in rl_builtin doc but found"
                     CURSYM*))))
            (SCAN)))))
        (GO WHILELABEL))
      (SCAN)
      (RETURN (REVERSIP L)))) 
(PUT 'RL_BUILTIN 'FORMFN 'RL_FORMBUILTIN) 
(PUT 'RL_FORMBUILTIN 'NUMBER-OF-ARGS 3) 
(DE RL_FORMBUILTIN (ARGL VARS MODE)
    (PROG (SPEC NAME DOC P SYNAL DESCRIPTION RETURNS ARGAL SEEALSOL DOCAL
           RLBUILTIN)
      (SETQ SPEC (CADR ARGL))
      (SETQ NAME (LTO_EATSOC 'NAME SPEC (LIST "missing name in " ARGL)))
      (SETQ DOC (LTO_EATSOC 'DOC SPEC (LIST "missing doc in" ARGL)))
      (PROG (X)
        (SETQ X DOC)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((AND (PAIRP X) (EQ (CAR X) 'SYNOPSIS))
             (PROG (W1)
               (SETQ W1
                       (CONS
                        (LTO_EATSOC 'POS (CDR X)
                                    (LIST "missing pos in" X "in" ARGL))
                        (LTO_EATSOC 'TEXT (CDR X)
                                    (LIST "missing text in" X "in" ARGL))))
               (SETQ SYNAL (CONS W1 SYNAL))
               (RETURN W1)))
            ((AND (PAIRP X) (EQ (CAR X) 'DESCRIPTION)) (SETQ DESCRIPTION X))
            ((AND (PAIRP X) (EQ (CAR X) 'RETURNS)) (SETQ RETURNS X))
            ((AND (PAIRP X) (EQ (CAR X) 'ARG))
             (PROG (W1)
               (SETQ W1
                       (CONS
                        (LTO_EATSOC 'POS (CDR X)
                                    (LIST "missing pos in" X "in" ARGL))
                        (CONS
                         (LTO_EATSOC 'NAME (CDR X)
                                     (LIST "missing name in" X "in" ARGL))
                         (LTO_EATSOC 'TEXT (CDR X)
                                     (LIST "missing text in" X "in" ARGL)))))
               (SETQ ARGAL (CONS W1 ARGAL))
               (RETURN W1)))
            ((AND (PAIRP X) (EQ (CAR X) 'SEEALSO))
             (PROG (W1)
               (SETQ W1 (CDR X))
               (SETQ SEEALSOL (CONS W1 SEEALSOL))
               (RETURN W1)))
            ((NOT (EQCAR X 'NAME))
             (REDERR (LIST "unknown keyword" (CAR X) "in" ARGL)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (W1)
        (SETQ W1 (CONS 'SYNOPSIS (REVERSIP SYNAL)))
        (SETQ DOCAL (CONS W1 DOCAL))
        (RETURN W1))
      (PROGN (SETQ DOCAL (CONS DESCRIPTION DOCAL)) DESCRIPTION)
      (PROGN (SETQ DOCAL (CONS RETURNS DOCAL)) RETURNS)
      (PROG (W1)
        (SETQ W1 (CONS 'ARGUMENTS (REVERSIP ARGAL)))
        (SETQ DOCAL (CONS W1 DOCAL))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (CONS 'SEEALSO (REVERSIP SEEALSOL)))
        (SETQ DOCAL (CONS W1 DOCAL))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (CONS 'DOC (REVERSIP DOCAL)))
        (SETQ RLBUILTIN (CONS W1 RLBUILTIN))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1
                (LIST 'SETQ 'RL_BUILTINS*
                      (LIST 'CONS (MKQUOTE NAME)
                            (LIST 'DELETE (MKQUOTE NAME) 'RL_BUILTINS*))))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE NAME) ''RL_SUPPORT ''RL_BUILTIN))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE NAME) ''RL_BUILTIN (MKQUOTE RLBUILTIN)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (RETURN (CONS 'PROGN (REVERSIP P))))) 
(PUT 'RL_BUILTINP 'NUMBER-OF-ARGS 1) 
(DE RL_BUILTINP (X) (AND (IDP X) (EQ (GET X 'RL_SUPPORT) 'RL_BUILTIN))) 
(ENDMODULE) 