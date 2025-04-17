(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RLBLACKBOX)) 
(REVISION 'RLBLACKBOX
          "$Id: rlblackbox.red 6030 2021-09-16 14:01:45Z thomas-sturm $") 
(COPYRIGHT 'RLBLACKBOX "(c) 2017 T. Sturm") 
(FLUID '(RL_BLACKBOXES*)) 
(FLUID '(RL_BBL*)) 
(PUT 'RL_BLACKBOX 'STAT 'RL_BLACKBOXSTAT) 
(PUT 'RL_BLACKBOXSTAT 'NUMBER-OF-ARGS 0) 
(DE RL_BLACKBOXSTAT NIL
    (PROG (SPEC)
      (SCAN)
      (COND
       ((NEQ CURSYM* '*LCBKT*)
        (REDERR (LIST "expecting '{' in rl_blackbox but found" CURSYM*))))
      (SETQ SPEC (RL_BLACKBOXSTATLIST))
      (SCAN)
      (RETURN (LIST 'RL_BLACKBOX SPEC)))) 
(PUT 'RL_BLACKBOXSTATLIST 'NUMBER-OF-ARGS 0) 
(DE RL_BLACKBOXSTATLIST NIL
    (PROG (SPEC KEY ENTRY)
      (SCAN)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ CURSYM* '*RCBKT*)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (LTO_ALPHAP CURSYM*))
           (REDERR
            (LIST "expecting alphabetic key in rl_service but found"
                  CURSYM*))))
         (SETQ KEY CURSYM*)
         (PROGN
          (SCAN)
          (COND
           ((NEQ CURSYM* 'EQUAL)
            (REDERR
             (LIST "expecting '=' in" "rl_blackbox" "but found" CURSYM*)))))
         (SCAN)
         (COND
          ((NOT (ATOM CURSYM*))
           (REDERR
            (LIST "expecting atomic entry in rl_blackbox but found" CURSYM*))))
         (SETQ ENTRY CURSYM*)
         (PROG (W1)
           (SETQ W1 (CONS KEY ENTRY))
           (SETQ SPEC (CONS W1 SPEC))
           (RETURN W1))
         (SCAN)
         (COND
          ((NEQ CURSYM* '*RCBKT*)
           (PROGN
            (COND
             ((NEQ CURSYM* '*COMMA*)
              (REDERR
               (LIST "expecting ',' or '}' in rl_blackbox but found"
                     CURSYM*))))
            (SCAN)))))
        (GO WHILELABEL))
      (RETURN (REVERSIP SPEC)))) 
(PUT 'RL_BLACKBOX 'FORMFN 'RL_FORMBLACKBOX) 
(PUT 'RL_FORMBLACKBOX 'NUMBER-OF-ARGS 3) 
(DE RL_FORMBLACKBOX (ARGL VARS M)
    (PROG (SPEC RL_B* RL_B P NAME VN ARGS DOC DOCAL N)
      (SETQ N 0)
      (SETQ SPEC (CADR ARGL))
      (SETQ NAME (LTO_EATSOC 'NAME SPEC (LIST "missing name in black box")))
      (SETQ N
              (LTO_EATSOC 'ARGNUM SPEC
                          (LIST "missing argnum in black box" NAME)))
      (SETQ DOC (LTO_EATSOC 'DOC SPEC (LIST "missing doc in black box" NAME)))
      (SETQ RL_B (INTERN (COMPRESS (NCONC (EXPLODE 'RL_) (EXPLODE NAME)))))
      (SETQ ARGS
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (MKID 'A I) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (MKID 'A I) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ RL_B* (INTERN (COMPRESS (NCONC (EXPLODE RL_B) (EXPLODE '*)))))
      (SETQ DOCAL
              (LIST (CONS 'SYNOPSIS (RL_DOCSYNOPSISBB RL_B N))
                    (CONS 'DESCRIPTION DOC)))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B) ''DOCAL (MKQUOTE DOCAL)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B) ''RL_SUPPORT ''RL_BLACKBOX))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'FLUID (MKQUOTE (LIST RL_B*))))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'SETQ 'RL_BBL* (LIST 'CONS (MKQUOTE RL_B*) 'RL_BBL*)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B) ''NUMBER-OF-ARGS N))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'DE RL_B ARGS (LIST 'APPLY RL_B* (CONS 'LIST ARGS))))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1
                (LIST 'SETQ 'RL_BLACKBOXES*
                      (LIST 'CONS (MKQUOTE RL_B) 'RL_BLACKBOXES*)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (RETURN (CONS 'PROGN (REVERSIP P))))) 
(PUT 'RL_DOCSYNOPSISBB 'NUMBER-OF-ARGS 2) 
(DE RL_DOCSYNOPSISBB (F N)
    (LTO_SCONCAT (LIST (LTO_AT2STR F) "/" (LTO_AT2STR N)))) 
(PUT 'RL_BLACKBOXP 'NUMBER-OF-ARGS 1) 
(DE RL_BLACKBOXP (X) (AND (IDP X) (EQ (GET X 'RL_SUPPORT) 'RL_BLACKBOX))) 
(ENDMODULE) 