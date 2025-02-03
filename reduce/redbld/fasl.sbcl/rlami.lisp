(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RLAMI)) 
(REVISION 'RLAMI "$Id: rlami.red 6030 2021-09-16 14:01:45Z thomas-sturm $") 
(COPYRIGHT 'RLAMI "(c) 1995-2009 A. Dolzmann, T. Sturm, 2017 T. Sturm") 
(PUT 'MIXEDPREFIXFORM 'ASSERT_DYNTYPECHK 'ATOMORPAIRP) 
(FLAG '(MIXEDPREFIXFORM) 'ASSERT_DYNTYPE) 
(PUT 'PSEUDOPREFIXFORM 'ASSERT_DYNTYPECHK 'ATOMORPAIRP) 
(FLAG '(PSEUDOPREFIXFORM) 'ASSERT_DYNTYPE) 
(PUT 'ATOMORPAIRP 'NUMBER-OF-ARGS 1) 
(PUT 'ATOMORPAIRP 'DEFINED-ON-LINE '42) 
(PUT 'ATOMORPAIRP 'DEFINED-IN-FILE 'REDLOG/RL/RLAMI.RED) 
(PUT 'ATOMORPAIRP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ATOMORPAIRP (X) (OR (ATOM X) (PAIRP X))) 
(PUT 'RL_MK*FOF 'NUMBER-OF-ARGS 1) 
(DE RL_MK*FOF (U) (RL_MK*FOF1 (RL_CSIMPL U))) 
(PUT 'RL_MK*FOF1 'NUMBER-OF-ARGS 1) 
(DE RL_MK*FOF1 (U)
    (COND ((OR (EQ U 'TRUE) (EQ U 'FALSE)) U)
          ((EQCAR U 'EQUAL) (RL_PREPFOF1 U))
          (T
           (CONS '*FOF
                 (CONS RL_CID*
                       (CONS U (COND (*RESUBS *SQVAR*) (T (LIST NIL))))))))) 
(PUT 'RL_REVAL 'NUMBER-OF-ARGS 2) 
(DE RL_REVAL (U V)
    (COND (V (RL_PREPFOF (RL_SIMP1 U))) (T (RL_MK*FOF (RL_SIMP1 U))))) 
(PUT 'RL_CSIMPL 'NUMBER-OF-ARGS 1) 
(DE RL_CSIMPL (U)
    (COND ((AND *RLSIMPL (GETD 'RL_SIMPL)) (RL_SIMPL U (LIST) (MINUS 1)))
          (T U))) 
(PUT 'RL_PREPFOF 'NUMBER-OF-ARGS 1) 
(DE RL_PREPFOF (F) (RL_PREPFOF1 (RL_CSIMPL F))) 
(PUT 'RL_PREPFOF1 'NUMBER-OF-ARGS 1) 
(DE RL_PREPFOF1 (F)
    (PROG (OP W)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN OP)))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (RETURN (LIST OP (CADR F) (RL_PREPFOF1 (CADDR F))))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL))
        (PROGN
         (COND
          ((NULL (SETQ W (GET (CAR RL_CID*) 'RL_PREPB)))
           (REDERR
            (LIST "current context" RL_USEDCNAME*
                  "does not support bounded quantifiers"))))
         (RETURN
          (LIST OP (CADR F) (APPLY W (LIST (CADDDR F)))
                (RL_PREPFOF1 (CADDR F)))))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (CONS OP
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (CDR F))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (RL_PREPFOF1 X)) (CAR X))
                                       NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (RL_PREPFOF1 X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN (APPLY (GET (CAR RL_CID*) 'RL_PREPAT) (LIST F))))) 
(PUT 'RL_CLEANUP 'NUMBER-OF-ARGS 2) 
(DE RL_CLEANUP (U V) (REVAL1 U V)) 
(PUT 'RL_SIMP 'NUMBER-OF-ARGS 1) 
(DE RL_SIMP (U) (RL_CSIMPL (RL_SIMP1 U))) 
(PUT 'RL_SIMP1 'NUMBER-OF-ARGS 1) 
(DE RL_SIMP1 (U)
    (PROG (W H)
      (COND ((NULL RL_CID*) (REDERR (LIST "select a context"))))
      (COND ((ATOM U) (RETURN (RL_SIMPATOM U))))
      ((LAMBDA (*STRICT_ARGCOUNT) (ARGNOCHK U)) T)
      (COND
       ((SETQ W (GET (CAR U) 'RL_SIMPFN))
        (RETURN
         (COND ((FLAGP W 'FULL) (APPLY W (LIST U)))
               (T (APPLY W (LIST (CDR U))))))))
      (COND
       ((SETQ W (GET (CAR U) (GET (CAR RL_CID*) 'SIMPFNNAME)))
        (RETURN
         (COND ((FLAGP W 'FULL) (APPLY W (LIST U)))
               (T (APPLY W (LIST (CDR U))))))))
      (COND
       ((SETQ W (GET (CAR U) 'PSOPFN))
        (PROGN
         (SETQ H (APPLY1 W (CDR U)))
         (COND ((NEQ H U) (RETURN (RL_SIMP1 H)))))))
      (COND
       ((FLAGP (CAR U) 'OPFN)
        (RETURN
         (RL_SIMP1
          (APPLY (CAR U)
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR U))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                         NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))))))
      (COND
       ((SETQ W (GET (CAR U) 'PREPFN2))
        (RETURN (RL_SIMP1 (APPLY W (LIST U))))))
      (SETQ H (IOTO_FORM2STR (CAR U)))
      (COND
       ((SETQ W (GET (CAR U) 'PRTCH))
        (SETQ H (LTO_SCONCAT (LIST (IOTO_FORM2STR W) " (" H ")")))))
      (REDMSG H "predicate")
      (PUT (CAR U) (GET (CAR RL_CID*) 'SIMPFNNAME)
           (GET (CAR RL_CID*) 'SIMPDEFAULT))
      (RETURN (RL_SIMP1 U)))) 
(PUT 'RL_SIMPATOM 'NUMBER-OF-ARGS 1) 
(DE RL_SIMPATOM (U)
    (PROG (W)
      (COND ((NULL U) (TYPERR "nil" "logical")))
      (COND ((NUMBERP U) (TYPERR (LIST "number" U) "logical")))
      (COND ((STRINGP U) (TYPERR (LIST "string" U) "logical")))
      (COND ((OR (EQ U 'TRUE) (EQ U 'FALSE)) (RETURN U)))
      (COND
       ((SETQ W (RL_GETTYPE U))
        (PROGN
         (COND
          ((OR (EQ W 'LOGICAL) (EQ W 'EQUATION) (EQ W 'SCALAR))
           (RETURN (RL_SIMP1 (CADR (GET U 'AVALUE))))))
         (TYPERR (LIST W U) "logical"))))
      (COND ((BOUNDP U) (RETURN (RL_SIMP1 (EVAL U)))))
      (TYPERR (LIST "unbound id" U) "logical"))) 
(PUT 'RL_SIMPBOP 'NUMBER-OF-ARGS 1) 
(DE RL_SIMPBOP (F)
    (CONS (CAR F)
          (PROG (X FORALL-RESULT FORALL-ENDPTR)
            (SETQ X (CDR F))
            (COND ((NULL X) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (X) (RL_SIMP1 X)) (CAR X)) NIL)))
           LOOPLABEL
            (SETQ X (CDR X))
            (COND ((NULL X) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (X) (RL_SIMP1 X)) (CAR X)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'RL_SIMPQ 'NUMBER-OF-ARGS 1) 
(DE RL_SIMPQ (F)
    (PROG (VL W)
      (SETQ VL (REVAL1 (CADR F) T))
      (COND ((EQCAR VL 'LIST) (SETQ VL (CDR VL))) (T (SETQ VL (LIST VL))))
      (SETQ W (RL_SIMP1 (CADDR F)))
      (PROG (X)
        (SETQ X (REVERSE VL))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (PROGN (RL_QVARCHK X) (SETQ W (LIST (CAR F) X W))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (FLAG VL 'USED*)
      (RETURN W))) 
(PUT 'RL_SIMPBQ 'NUMBER-OF-ARGS 1) 
(DE RL_SIMPBQ (F)
    (PROG (SIMPB X)
      (COND
       ((NULL (SETQ SIMPB (GET (CAR RL_CID*) 'RL_SIMPB)))
        (REDERR
         (LIST "current context" RL_USEDCNAME*
               "does not support bounded quantifiers"))))
      (SETQ X (REVAL1 (CADR F) T))
      (COND ((NOT (IDP X)) (TYPERR X "bounded quantified variable")))
      (FLAG (LIST X) 'USED*)
      (RETURN
       (LIST (CAR F) X (RL_SIMP1 (CADDDR F))
             (APPLY SIMPB (LIST (CADDR F) X)))))) 
(PUT 'RL_QVARCHK 'NUMBER-OF-ARGS 1) 
(DE RL_QVARCHK (V)
    (COND
     ((OR (NOT (SFTO_KERNELP V)) (AND *RLBRKCXK (PAIRP V)))
      (TYPERR V "quantified variable")))) 
(PUT 'RL_SIMP*FOF 'NUMBER-OF-ARGS 1) 
(DE RL_SIMP*FOF (U)
    (PROG (TAG F W)
      (COND ((CADDR U) (RETURN (CADR U))))
      (SETQ TAG (CAR U))
      (SETQ F (CADR U))
      (COND
       ((NEQ TAG RL_CID*)
        (PROGN
         ((LAMBDA (*MSG) (SETQ W (RL_SET TAG))) NIL)
         (SETQ F (RL_PREPFOF F))
         ((LAMBDA (*MSG) (RL_SET W)) NIL)
         (RETURN (RL_SIMP F)))))
      (RETURN (RL_RESIMP F)))) 
(PUT 'RL_RESIMP 'NUMBER-OF-ARGS 1) 
(DE RL_RESIMP (U)
    (PROG (OP W)
      (SETQ OP (COND ((ATOM U) U) (T (CAR U))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN U)))
      (COND
       ((OR (EQ OP 'EX) (EQ OP 'ALL))
        (PROGN
         (COND
          ((SETQ W (RL_GETTYPE (CADR U)))
           (TYPERR (LIST W (CADR U)) "quantified variable")))
         (RL_QVARCHK (CADR U))
         (RETURN (LIST OP (CADR U) (RL_RESIMP (CADDR U)))))))
      (COND
       ((OR (EQ OP 'BEX) (EQ OP 'BALL))
        (PROGN
         (COND
          ((SETQ W (RL_GETTYPE (CADR U)))
           (TYPERR (LIST W (CADR U)) "quantified variable")))
         (RL_QVARCHK (CADR U))
         (COND
          ((NULL (SETQ W (GET (CAR RL_CID*) 'RL_RESIMPB)))
           (REDERR
            (LIST "current context" RL_USEDCNAME*
                  "does not support bounded quantifiers"))))
         (RETURN
          (LIST (COND ((ATOM U) U) (T (CAR U))) (CADR U) (RL_RESIMP (CADDR U))
                (APPLY W (LIST (CADDDR U))))))))
      (COND
       ((OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
            (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
        (RETURN
         (CONS OP
               (PROG (X FORALL-RESULT FORALL-ENDPTR)
                 (SETQ X (CDR U))
                 (COND ((NULL X) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (X) (RL_RESIMP X)) (CAR X))
                                       NIL)))
                LOOPLABEL
                 (SETQ X (CDR X))
                 (COND ((NULL X) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (RL_RESIMP X)) (CAR X)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))))
      (RETURN (APPLY (GET (CAR RL_CID*) 'RL_RESIMPAT) (LIST U))))) 
(PUT 'RL_GETTYPE 'NUMBER-OF-ARGS 1) 
(DE RL_GETTYPE (V)
    ((LAMBDA (W) (COND (W (CAR W)) (T (GET V 'RTYPE)))) (GET V 'AVALUE))) 
(PUT 'RL_LENGTHLOGICAL 'NUMBER-OF-ARGS 1) 
(DE RL_LENGTHLOGICAL (U) (RL_LENGTHFOF (RL_SIMP U))) 
(PUT 'RL_LENGTHFOF 'NUMBER-OF-ARGS 1) 
(PUT 'RL_LENGTHFOF 'DEFINED-ON-LINE '243) 
(PUT 'RL_LENGTHFOF 'DEFINED-IN-FILE 'REDLOG/RL/RLAMI.RED) 
(PUT 'RL_LENGTHFOF 'PROCEDURE_TYPE '(ARROW FORMULA INTEGER)) 
(DE RL_LENGTHFOF (F)
    (PROG (OP)
      (SETQ OP (COND ((ATOM F) F) (T (CAR F))))
      (COND ((OR (EQ OP 'TRUE) (EQ OP 'FALSE)) (RETURN 1)))
      (COND ((OR (EQ OP 'EX) (EQ OP 'ALL)) (RETURN 2)))
      (COND ((OR (EQ OP 'BEX) (EQ OP 'BALL)) (RETURN 3)))
      (COND
       ((OR (OR (EQ OP 'TRUE) (EQ OP 'FALSE))
            (OR (OR (OR (EQ OP 'OR) (EQ OP 'AND)) (EQ OP 'NOT))
                (OR (EQ OP 'IMPL) (EQ OP 'REPL) (EQ OP 'EQUIV)))
            (OR (EQ OP 'EX) (EQ OP 'ALL)) (OR (EQ OP 'BEX) (EQ OP 'BALL)))
        (RETURN (LENGTH (CDR F)))))
      (RETURN (APPLY (GET (CAR RL_CID*) 'RL_LENGTHAT) (LIST F))))) 
(PUT 'RL_SUB*FOF 'NUMBER-OF-ARGS 2) 
(DE RL_SUB*FOF (AL F) (RL_MK*FOF (RL_SUBFOF AL (RL_SIMP F)))) 
(SETQ RL_BUILTINS* (CONS 'MKOR (DELETE 'MKOR RL_BUILTINS*))) 
(PUT 'MKOR 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'MKOR 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "for R mkor E"))
        (DESCRIPTION . "for-loop action for constructing disjunctions")
        (RETURNS . "Any")
        (ARGUMENTS
         (1 "R"
          . "range specification as documented in Sect.5.4 of the REDUCE manual ")
         (2 "E" . "RLISP expression"))
        (SEEALSO)))) 
(SETQ FORACTIONS* (LTO_INSERTQ 'MKOR FORACTIONS*)) 
(PUT 'MKOR 'INITVAL ''FALSE) 
(PUT 'MKOR 'BIN 'RL_MKOR) 
(FLAG '(RL_MKOR) 'OPFN) 
(PUT 'RL_MKOR 'NUMBER-OF-ARGS 2) 
(DE RL_MKOR (A B)
    (COND
     ((EQ *MODE 'SYMBOLIC) (REDERR "`for ... mkor' invalid in symbolic mode"))
     ((NULL A) (REDERR "empty body in `for ... mkor'")) ((EQ B 'FALSE) A)
     (T
      (PROGN
       (SETQ A (COND ((EQCAR A 'OR) (CDR A)) (T (LIST A))))
       (SETQ B (COND ((EQCAR B 'OR) (CDR B)) (T (LIST B))))
       (CONS 'OR (NCONC B A)))))) 
(SETQ RL_BUILTINS* (CONS 'MKAND (DELETE 'MKAND RL_BUILTINS*))) 
(PUT 'MKAND 'RL_SUPPORT 'RL_BUILTIN) 
(PUT 'MKAND 'RL_BUILTIN
     '((DOC (SYNOPSIS (1 . "for R mkand E"))
        (DESCRIPTION . "for-loop action for constructing conjunctions")
        (RETURNS . "Any")
        (ARGUMENTS
         (1 "R"
          . "range specification as documented in Sect.5.4 of the REDUCE manual ")
         (2 "E" . "RLISP expression"))
        (SEEALSO)))) 
(SETQ FORACTIONS* (LTO_INSERTQ 'MKAND FORACTIONS*)) 
(PUT 'MKAND 'INITVAL ''TRUE) 
(PUT 'MKAND 'BIN 'RL_MKAND) 
(FLAG '(RL_MKAND) 'OPFN) 
(PUT 'RL_MKAND 'NUMBER-OF-ARGS 2) 
(DE RL_MKAND (A B)
    (COND
     ((EQ *MODE 'SYMBOLIC) (REDERR "`for ... mkand' invalid in symbolic mode"))
     ((NULL A) (REDERR "empty body in `for ... mkand'")) ((EQ B 'TRUE) A)
     (T
      (PROGN
       (SETQ A (COND ((EQCAR A 'AND) (CDR A)) (T (LIST A))))
       (SETQ B (COND ((EQCAR B 'AND) (CDR B)) (T (LIST B))))
       (CONS 'AND (NCONC B A)))))) 
(ENDMODULE) 