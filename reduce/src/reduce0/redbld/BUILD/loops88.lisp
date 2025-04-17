(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'LOOPS88)) 
(FLUID '(*BLOCKP LOOPDELIMSLIST*)) 
(GLOBAL '(CURSYM* REPEATKEYWORDS* WHILEKEYWORDS*)) 
(SETQ REPEATKEYWORDS* '(FINALLY INITIALLY RETURNS UNTIL WITH)) 
(PUT 'REPEATSTAT88 'NUMBER-OF-ARGS 0) 
(PUT 'REPEATSTAT88 'DEFINED-ON-LINE '39) 
(PUT 'REPEATSTAT88 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'REPEATSTAT88 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE REPEATSTAT88 NIL
    (PROG (BODY *BLOCKP X Y Z)
      (SETQ LOOPDELIMSLIST* (CONS REPEATKEYWORDS* LOOPDELIMSLIST*))
      (FLAG REPEATKEYWORDS* 'DELIM)
      (SETQ BODY (ERROREVAL '(XREAD T)))
      (COND ((NOT (MEMQ CURSYM* REPEATKEYWORDS*)) (SYMERR 'REPEAT T)))
     A
      (SETQ X CURSYM*)
      (SETQ Y
              (ERROREVAL
               (COND ((EQ X 'WITH) '(XREAD 'LAMBDA)) (T '(XREAD T)))))
      (SETQ Z (CONS (CONS X Y) Z))
      (COND ((MEMQ CURSYM* REPEATKEYWORDS*) (GO A)))
      (REMFLAG (CAR LOOPDELIMSLIST*) 'DELIM)
      (SETQ LOOPDELIMSLIST* (CDR LOOPDELIMSLIST*))
      (COND (LOOPDELIMSLIST* (FLAG (CAR LOOPDELIMSLIST*) 'DELIM)))
      (RETURN (CONS 'REPEAT (CONS BODY (REVERSIP Z)))))) 
(PUT 'REPEAT88 'DEFINED-ON-LINE '56) 
(PUT 'REPEAT88 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'REPEAT88 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DM REPEAT88 (U)
    (PROG (BODY LAB XWITH)
      (SETQ BODY (CADR U))
      (SETQ U (CDDR U))
      (SETQ XWITH (ATSOC 'WITH U))
      (RETURN
       (SUBLIS
        (PAIR '($LOCALS $DO $RETS $INITS $FINS $BOOL $LABEL)
              (LIST (COND (XWITH (CDR XWITH)) (T NIL)) BODY
                    (X-CAR (X-CDR (ATSOC 'RETURNS U)))
                    (MKFN (X-CDR (ATSOC 'INITIALLY U)) 'PROGN)
                    (MKFN (X-CDR (ATSOC 'FINALLY U)) 'PROGN)
                    (X-CAR (X-CDR (ATSOC 'UNTIL U))) (GENSYM)))
        '(PROG $LOCALS
          $INITS
          $LABEL
          $DO
           (COND ($BOOL $FINS (RETURN $RETS)))
           (GO $LABEL)))))) 
(PUT 'REMCOMMA* 'NUMBER-OF-ARGS 1) 
(PUT 'REMCOMMA* 'DEFINED-ON-LINE '76) 
(PUT 'REMCOMMA* 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'REMCOMMA* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REMCOMMA* (U) (COND ((NULL U) NIL) (T (REMCOMMA (CDR U))))) 
(PUT 'X-CAR 'NUMBER-OF-ARGS 1) 
(PUT 'X-CAR 'DEFINED-ON-LINE '78) 
(PUT 'X-CAR 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'X-CAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE X-CAR (U) (COND ((ATOM U) U) (T (CAR U)))) 
(PUT 'X-CDR 'NUMBER-OF-ARGS 1) 
(PUT 'X-CDR 'DEFINED-ON-LINE '80) 
(PUT 'X-CDR 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'X-CDR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE X-CDR (U) (COND ((NULL U) NIL) (T (LIST (CDR U))))) 
(PUT 'FORMREPEAT88 'NUMBER-OF-ARGS 3) 
(PUT 'FORMREPEAT88 'DEFINED-ON-LINE '84) 
(PUT 'FORMREPEAT88 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'FORMREPEAT88 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMREPEAT88 (U VARS MODE)
    (PROG (Y Z)
      (PROG (X)
        (SETQ X (CDDR U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((EQ (CAR X) 'WITH)
             (PROGN
              (SETQ Y (REMCOMMA (CDR X)))
              (SETQ VARS
                      (NCONC
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J Y)
                         (COND ((NULL J) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J) (CONS J 'SCALAR))
                                           (CAR J))
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (CDR J))
                         (COND ((NULL J) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (CONS J 'SCALAR)) (CAR J))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       VARS))
              (SETQ Z (CONS (CONS (CAR X) Y) Z))))
            (T (SETQ Z (CONS (CONS (CAR X) (FORMC (CDR X) VARS MODE)) Z)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CONS 'REPEAT (CONS (FORMC (CADR U) VARS MODE) (REVERSIP Z)))))) 
(SETQ WHILEKEYWORDS* '(COLLECT DO FINALLY INITIALLY RETURNS WITH)) 
(PUT 'WHILSTAT88 'NUMBER-OF-ARGS 0) 
(PUT 'WHILSTAT88 'DEFINED-ON-LINE '103) 
(PUT 'WHILSTAT88 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'WHILSTAT88 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE WHILSTAT88 NIL
    (PROG (*BLOCKP BOOL1 X Y Z)
      (SETQ LOOPDELIMSLIST* (CONS WHILEKEYWORDS* LOOPDELIMSLIST*))
      (FLAG WHILEKEYWORDS* 'DELIM)
      (SETQ BOOL1 (ERROREVAL '(XREAD T)))
      (COND ((NOT (MEMQ CURSYM* WHILEKEYWORDS*)) (SYMERR 'WHILE T)))
     A
      (SETQ X CURSYM*)
      (SETQ Y
              (ERROREVAL
               (COND ((EQ X 'WITH) '(XREAD 'LAMBDA)) (T '(XREAD T)))))
      (SETQ Z (CONS (CONS X Y) Z))
      (COND ((MEMQ CURSYM* WHILEKEYWORDS*) (GO A)))
      (REMFLAG (CAR LOOPDELIMSLIST*) 'DELIM)
      (SETQ LOOPDELIMSLIST* (CDR LOOPDELIMSLIST*))
      (COND (LOOPDELIMSLIST* (FLAG (CAR LOOPDELIMSLIST*) 'DELIM)))
      (RETURN (CONS 'WHILE (CONS BOOL1 (REVERSIP Z)))))) 
(PUT 'WHILE88 'DEFINED-ON-LINE '120) 
(PUT 'WHILE88 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'WHILE88 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DM WHILE88 (U)
    (PROG (BODY BOOL LAB RETS VARS)
      (SETQ BOOL (CADR U))
      (SETQ U (CDDR U))
      (SETQ RETS (X-CAR (X-CDR (ATSOC 'RETURNS U))))
      (SETQ VARS (X-CAR (X-CDR (ATSOC 'WITH U))))
      (COND
       ((SETQ BODY (ATSOC 'COLLECT U))
        (PROGN
         (SETQ VARS (CONS (GENSYM) VARS))
         (SETQ BODY (LIST 'SETQ (CAR VARS) (LIST 'CONS (CDR BODY) (CAR VARS))))
         (COND (RETS (REDERR "While loop value conflict")))
         (SETQ RETS (LIST 'REVERSIP (CAR VARS)))))
       ((SETQ BODY (ATSOC 'DO U)) (SETQ BODY (CDR BODY)))
       (T (REDERR "Missing body in WHILE statement")))
      (RETURN
       (SUBLIS
        (PAIR '($LOCALS $DO $RETS $INITS $FINS $BOOL $LABEL)
              (LIST VARS BODY RETS (MKFN (X-CDR (ATSOC 'INITIALLY U)) 'PROGN)
                    (MKFN (X-CDR (ATSOC 'FINALLY U)) 'PROGN) BOOL (GENSYM)))
        '(PROG $LOCALS
          $INITS
          $LABEL
           (COND ((NOT $BOOL) $FINS (RETURN $RETS)))
          $DO
           (GO $LABEL)))))) 
(PUT 'FORMWHILE88 'NUMBER-OF-ARGS 3) 
(PUT 'FORMWHILE88 'DEFINED-ON-LINE '153) 
(PUT 'FORMWHILE88 'DEFINED-IN-FILE 'RLISP88/LOOPS88.RED) 
(PUT 'FORMWHILE88 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMWHILE88 (U VARS MODE)
    (PROG (Y Z)
      (PROG (X)
        (SETQ X (CDDR U))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (COND
            ((EQ (CAR X) 'WITH)
             (PROGN
              (SETQ Y (REMCOMMA (CDR X)))
              (SETQ VARS
                      (NCONC
                       (PROG (J FORALL-RESULT FORALL-ENDPTR)
                         (SETQ J Y)
                         (COND ((NULL J) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (J) (CONS J 'SCALAR))
                                           (CAR J))
                                          NIL)))
                        LOOPLABEL
                         (SETQ J (CDR J))
                         (COND ((NULL J) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (CONS J 'SCALAR)) (CAR J))
                                       NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))
                       VARS))
              (SETQ Z (CONS (CONS (CAR X) Y) Z))))
            (T (SETQ Z (CONS (CONS (CAR X) (FORMC (CDR X) VARS MODE)) Z)))))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (CONS 'WHILE (CONS (FORMC (CADR U) VARS MODE) (REVERSIP Z)))))) 
(ENDMODULE) 