(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RLTYPE)) 
(REVISION 'RLTYPE "$Id: rltype.red 6030 2021-09-16 14:01:45Z thomas-sturm $") 
(COPYRIGHT 'RLTYPE "(c) 2016-2020 T. Sturm") 
(FLUID '(MODE*)) 
(PUT 'RL_TYPE 'STAT 'RL_TYPESTAT) 
(PUT 'RL_TYPESTAT 'NUMBER-OF-ARGS 0) 
(DE RL_TYPESTAT NIL
    (PROG (L KEY ENTRY)
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* '*LCBKT*)
         (REDERR (LIST "expecting '{' in, " "rl_type" "but found" CURSYM*)))))
      (SCAN)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ CURSYM* '*RCBKT*)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (IDP CURSYM*))
           (REDERR
            (LIST "expecting identifier in rl_type but found" CURSYM*))))
         (SETQ KEY CURSYM*)
         (SCAN)
         (COND
          ((NEQ CURSYM* 'EQUAL)
           (PROG (W1) (SETQ W1 (CONS KEY T)) (SETQ L (CONS W1 L)) (RETURN W1)))
          (T
           (PROGN
            (COND ((EQ KEY 'DOC) (SETQ ENTRY (RL_TYPESTATDOC)))
                  ((MEMQ KEY '(NAME INHERITS))
                   (PROGN
                    (SCAN)
                    (COND
                     ((NOT (IDP CURSYM*))
                      (REDERR
                       (LIST "expecting identifier in rl_type but found"
                             CURSYM*))))
                    (SETQ ENTRY CURSYM*)
                    (SCAN)))
                  ((MEMQ KEY '(A2S S2A)) (SETQ ENTRY (XREAD T)))
                  (T (REDERR (LIST "unknown keyword" KEY "in rl_type"))))
            (PROG (W1)
              (SETQ W1 (CONS KEY ENTRY))
              (SETQ L (CONS W1 L))
              (RETURN W1))
            NIL)))
         (COND
          ((NEQ CURSYM* '*RCBKT*)
           (PROGN
            (COND
             ((NEQ CURSYM* '*COMMA*)
              (REDERR
               (LIST "expecting ',' or '}' in rl_type but found" CURSYM*))))
            (SCAN)))))
        (GO WHILELABEL))
      (SCAN)
      (RETURN (LIST 'RL_TYPE (REVERSIP L))))) 
(PUT 'RL_TYPESTATDOC 'NUMBER-OF-ARGS 0) 
(DE RL_TYPESTATDOC NIL
    (PROG (KEY ENTRY L)
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* '*LCBKT*)
         (REDERR
          (LIST "expecting '{' in, " "rl_type doc" "but found" CURSYM*)))))
      (SCAN)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ CURSYM* '*RCBKT*)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (IDP CURSYM*))
           (REDERR
            (LIST "expecting identifier in rl_type doc but found" CURSYM*))))
         (SETQ KEY CURSYM*)
         (COND
          ((MEMQ KEY '(DESCRIPTION SYNTAX SEMANTICS URL EXAMPLE))
           (PROGN
            (PROGN
             (SCAN)
             (COND
              ((NEQ CURSYM* 'EQUAL)
               (REDERR
                (LIST "expecting '=' in" "rl_type" "but found" CURSYM*)))))
            (SCAN)
            (COND
             ((NOT (STRINGP CURSYM*))
              (REDERR
               (LIST "expecting a string in rl_type doc but found" CURSYM*))))
            (SETQ ENTRY CURSYM*)
            (SCAN)))
          (T (REDERR (LIST "unknown keyword" KEY "in rl_type doc"))))
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
               (LIST "expecting ',' or '}' in rl_type doc but found"
                     CURSYM*))))
            (SCAN)))))
        (GO WHILELABEL))
      (SCAN)
      (RETURN (REVERSIP L)))) 
(PUT 'RL_TYPE 'FORMFN 'RL_FORMTYPE) 
(PUT 'RL_FORMTYPE 'NUMBER-OF-ARGS 3) 
(DE RL_FORMTYPE (ARGL VARS MODE)
    (PROG (SPEC NAME P RLTYPE)
      (SETQ SPEC (CADR ARGL))
      (SETQ NAME (LTO_EATSOC 'NAME SPEC (LIST "missing type name in" ARGL)))
      (PROG (X)
        (SETQ X SPEC)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((AND (PAIRP X) (MEMQ (CAR X) '(A2S S2A DOC INHERITS EQUATIONAL)))
             (PROGN (SETQ RLTYPE (CONS X RLTYPE)) X))
            ((NOT (EQCAR X 'NAME))
             (REDERR (LIST "unknown keyword" (CAR X) "in" ARGL)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE NAME) ''RL_SUPPORT ''RL_TYPE))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE NAME) ''RL_TYPE (MKQUOTE RLTYPE)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (RETURN (CONS 'PROGN (REVERSIP P))))) 
(PUT 'RL_TYPEP 'NUMBER-OF-ARGS 1) 
(DE RL_TYPEP (X) (AND (IDP X) (EQ (GET X 'RL_SUPPORT) 'RL_TYPE))) 
(PUT 'RL_TYPEEQUATIONALP 'NUMBER-OF-ARGS 1) 
(DE RL_TYPEEQUATIONALP (TYPE)
    (PROG (W)
      (SETQ W (ATSOC 'EQUATIONAL (GET TYPE 'RL_TYPE)))
      (RETURN (AND (PAIRP W) (CDR W))))) 
(PUT 'RL_TYPEA2S 'NUMBER-OF-ARGS 1) 
(DE RL_TYPEA2S (TYPE) (RL_TYPEENTRY TYPE 'A2S)) 
(PUT 'RL_TYPES2A 'NUMBER-OF-ARGS 1) 
(DE RL_TYPES2A (TYPE) (RL_TYPEENTRY TYPE 'S2A)) 
(PUT 'RL_TYPEDOC 'NUMBER-OF-ARGS 1) 
(DE RL_TYPEDOC (TYPE) (RL_TYPEENTRY TYPE 'DOC)) 
(PUT 'RL_TYPEINHERITS 'NUMBER-OF-ARGS 1) 
(DE RL_TYPEINHERITS (TYPE) (RL_TYPEENTRY TYPE 'INHERITS)) 
(PUT 'RL_TYPEENTRY 'NUMBER-OF-ARGS 2) 
(DE RL_TYPEENTRY (TYPE ENTRY)
    (PROG (W)
      (SETQ W (ATSOC ENTRY (GET TYPE 'RL_TYPE)))
      (RETURN (COND ((PAIRP W) (CDR W)))))) 
(PUT 'RL_TYPEARITY 'NUMBER-OF-ARGS 1) 
(DE RL_TYPEARITY (TYPE)
    (PROG (AR)
      (SETQ AR
              (COND ((EQ TYPE 'ENUM) 'N)
                    (T
                     (OR
                      (GET (OR (RL_TYPEA2S TYPE) (RL_TYPES2A TYPE))
                           'NUMBER-OF-ARGS)
                      '?))))
      (COND ((FIXP AR) (SETQ AR (DIFFERENCE AR 1))))
      (RETURN AR))) 
(ENDMODULE) 