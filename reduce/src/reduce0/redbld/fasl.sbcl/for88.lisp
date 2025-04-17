(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'FOR88)) 
(FLUID '(*FASTFOR BINOPS* LOOPDELIMSLIST*)) 
(GLOBAL '(FORKEYWORDS*)) 
(FLAG '(FASTFOR) 'SWITCH) 
(DEFLIST '((ALL FORALLSTAT)) 'FORLOOPS*) 
(SETQ FORKEYWORDS*
        '(COLLECT COUNT DO EACH EVERY FINALLY IN INITIALLY JOIN ON PRODUCT
          RETURNS SOME STEP SUM UNLESS UNTIL WHEN WITH MAXIMIZE MINIMIZE)) 
(REMFLAG FORKEYWORDS* 'DELIM) 
(SETQ FORBINOPS*
        '((APPEND APPEND) (COLLECT CONS) (COUNT PLUS2) (JOIN NCONC)
          (MAXIMIZE MAX2*) (MINIMIZE MIN2*) (PRODUCT TIMES2) (SUM PLUS2))) 
(GLOBAL '(CURSYM*)) 
(PUT 'FORSTAT88 'NUMBER-OF-ARGS 0) 
(PUT 'FORSTAT88 'DEFINED-ON-LINE '79) 
(PUT 'FORSTAT88 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'FORSTAT88 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FORSTAT88 NIL
    (PROG (*BLOCKP X)
      (COND ((SETQ X (GET (SCAN) 'FORLOOPS*)) (RETURN (LISPAPPLY X NIL))))
      (SETQ LOOPDELIMSLIST* (CONS FORKEYWORDS* LOOPDELIMSLIST*))
      (FLAG FORKEYWORDS* 'DELIM)
      (RETURN
       (CONS 'FOR
             (COND
              ((NEQ CURSYM* 'EACH)
               (PROGN (SETQ X (FORFRAG)) (CONS X (FORTAIL))))
              (T (FORTAIL))))))) 
(PUT 'FORFRAG 'NUMBER-OF-ARGS 0) 
(PUT 'FORFRAG 'DEFINED-ON-LINE '89) 
(PUT 'FORFRAG 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'FORFRAG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FORFRAG NIL
    (PROG (INCR VAR X)
      (SETQ X (ERROREVAL '(XREAD1 'FOR)))
      (COND
       ((OR (NOT (EQCAR X 'SETQ)) (NOT (IDP (SETQ VAR (CADR X)))))
        (SYMERR 'FOR T)))
      (SETQ X (CADDR X))
      (COND
       ((EQ CURSYM* 'STEP)
        (PROGN
         (SETQ INCR (ERROREVAL '(XREAD T)))
         (COND ((NOT (EQ CURSYM* 'UNTIL)) (SYMERR 'FOR T)))))
       ((EQ CURSYM* '*COLON*) (SETQ INCR 1)) (T (SYMERR 'FOR T)))
      (RETURN (LIST 'INCR VAR X (ERROREVAL '(XREAD T)) INCR)))) 
(PUT 'ERROREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'ERROREVAL 'DEFINED-ON-LINE '109) 
(PUT 'ERROREVAL 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'ERROREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERROREVAL (U)
    (PROG (X)
      (SETQ X (ERRORSET* U T))
      (COND ((ERRORP X) (ERROR1)) (T (RETURN (CAR X)))))) 
(PUT 'EACHFRAG 'NUMBER-OF-ARGS 0) 
(PUT 'EACHFRAG 'DEFINED-ON-LINE '115) 
(PUT 'EACHFRAG 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'EACHFRAG 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EACHFRAG NIL
    (PROG (X Y)
      (COND
       ((OR (NOT (IDP (SETQ X (SCAN)))) (NOT (MEMQ (SETQ Y (SCAN)) '(IN ON))))
        (SYMERR "For each" T)))
      (RETURN (LIST Y X (ERROREVAL '(XREAD T)))))) 
(PUT 'FORTAIL 'NUMBER-OF-ARGS 0) 
(PUT 'FORTAIL 'DEFINED-ON-LINE '122) 
(PUT 'FORTAIL 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'FORTAIL 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE FORTAIL NIL
    (PROG (X Y Z Z1)
     A
      (SETQ Z1 CURSYM*)
      (COND
       ((EQ Z1 'EACH)
        (COND
         ((OR (NOT (IDP (SETQ X (SCAN))))
              (NOT (MEMQ (SETQ Y (SCAN)) '(IN ON))))
          (SYMERR "FOR EACH" T))
         (T
          (PROGN (SETQ Z (CONS (LIST Y X (ERROREVAL '(XREAD T))) Z)) (GO A)))))
       ((EQ Z1 'WITH) (SETQ Z (CONS (CONS Z1 (ERROREVAL '(XREAD 'LAMBDA))) Z)))
       ((EQ Z1 '*SEMICOL*) (SYMERR "FOR EACH" T))
       (T (SETQ Z (CONS (CONS Z1 (ERROREVAL '(XREAD T))) Z))))
      (COND ((MEMQ CURSYM* FORKEYWORDS*) (GO A)))
      (REMFLAG (CAR LOOPDELIMSLIST*) 'DELIM)
      (SETQ LOOPDELIMSLIST* (CDR LOOPDELIMSLIST*))
      (COND (LOOPDELIMSLIST* (FLAG (CAR LOOPDELIMSLIST*) 'DELIM)))
      (RETURN (REVERSIP Z)))) 
(PUT 'FORMFOR88 'NUMBER-OF-ARGS 3) 
(PUT 'FORMFOR88 'DEFINED-ON-LINE '142) 
(PUT 'FORMFOR88 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'FORMFOR88 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE FORMFOR88 (U VARS MODE)
    (PROG (X Y Z)
      (SETQ U (SETQ Z (CDR U)))
     A
      (COND ((NULL Z) (GO B)))
      (SETQ X (CAR Z))
      (COND
       ((MEMQ (CAR X) '(DOWN FROM INCR IN ON))
        (SETQ VARS (CONS (CONS (CADR X) 'SCALAR) VARS))))
      (COND ((NULL (EQ (CAR X) 'WITH)) (PROGN (SETQ Z (CDR Z)) (GO A))))
      (SETQ X (REMCOMMA (CDR X)))
     A0
      (COND
       (X
        (PROGN
         (SETQ Y (CONS (CONS (CAR X) 'SCALAR) Y))
         (SETQ X (CDR X))
         (GO A0))))
      (SETQ VARS (NCONC (REVERSIP* Y) VARS))
      (SETQ Z (CDR Z))
      (GO A)
     B
      (COND ((NULL U) (RETURN (CONS 'FOR (REVERSIP Z)))))
      (SETQ X (CAR U))
      (COND
       ((MEMQ (CAR X) '(DOWN FROM INCR))
        (SETQ Z
                (CONS
                 (CONS (CAR X) (CONS (CADR X) (FORMCLIS (CDDR X) VARS MODE)))
                 Z)))
       ((EQ (CAR X) 'WITH) (SETQ Z (CONS (CONS (CAR X) (REMCOMMA (CDR X))) Z)))
       ((MEMQ (CAR X) '(IN ON))
        (SETQ Z
                (CONS
                 (CONS (CAR X) (LIST (CADR X) (FORMC (CADDR X) VARS MODE)))
                 Z)))
       (T (SETQ Z (CONS (CONS (CAR X) (FORMC (CDR X) VARS MODE)) Z))))
      (SETQ U (CDR U))
      (GO B))) 
(PUT 'FOR88 'DEFINED-ON-LINE '170) 
(PUT 'FOR88 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'FOR88 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DM FOR88 (X)
    (PROG (LVARS INIT INIT2 FINAL BODY $COND RETS CUR $WHEN *MAXMINFLAG NEXT
           $LABEL2 $WHILE CX IV ACTION CURVAR VALUEVAR Y)
      (SETQ X (CDR X))
      (SETQ ACTION (CAAR X))
      (SETQ $LABEL2 (GENSYM))
     LOOP
      (COND
       ((NULL X)
        (PROGN
         (SETQ FINAL (MKFN FINAL 'PROGN))
         (SETQ NEXT (MKFN NEXT 'PROGN))
         (SETQ $COND (MKFN $COND 'OR))
         (SETQ CUR (MKFN CUR 'PROGN))
         (SETQ BODY (MKFN BODY 'PROGN))
         (COND
          ($WHILE
           (SETQ $WHILE
                   (FORCOND
                    (SUBLIS
                     (PAIR '($WHILE FINAL RETS)
                           (LIST (MKFN $WHILE 'OR) FINAL RETS))
                     '($WHILE FINAL (RETURN RETS)))))))
         (COND ($WHEN (SETQ BODY (FORCOND (LIST $WHEN BODY)))))
         (COND (*MAXMINFLAG (SETQ RETS (LIST 'NULL2ZERO RETS))))
         (RETURN
          (FORPROG
           (CONS LVARS
                 (NCONC INIT
                        (NCONC INIT2
                               (SUBLIS
                                (PAIR
                                 '(FINAL BODY $COND RETS CUR NEXT $LABEL
                                   $LABEL2 $WHILE)
                                 (LIST FINAL BODY $COND RETS CUR NEXT (GENSYM)
                                       $LABEL2 $WHILE))
                                (COND
                                 (FINAL
                                  '($LABEL
                                    (COND ($COND (PROGN FINAL (RETURN RETS))))
                                    CUR $WHILE BODY $LABEL2 NEXT (GO $LABEL)))
                                 (T
                                  '($LABEL (COND ($COND (RETURN RETS))) CUR
                                    $WHILE BODY $LABEL2 NEXT
                                    (GO $LABEL)))))))))))))
      (SETQ CX (CAR X))
      (COND ((ATOM CX) (REDERR (LIST CX "invalid in FOR form")))
            ((EQ (CAR CX) 'WITH) (SETQ LVARS (APPEND LVARS (CDR CX))))
            ((EQ (CAR CX) 'INITIALLY) (SETQ INIT (ACONC INIT (CDR CX))))
            ((EQ (CAR CX) 'FINALLY) (SETQ FINAL (ACONC FINAL (CDR CX))))
            ((EQ (CAR CX) 'ON)
             (PROGN
              (SETQ VALUEVAR (CADR CX))
              (SETQ LVARS (CONS VALUEVAR LVARS))
              (SETQ $COND (CONS (LIST 'NULL VALUEVAR) $COND))
              (SETQ INIT (CONS (LIST 'SETQ VALUEVAR (CADDR CX)) INIT))
              (COND
               ((CDDDR CX)
                (SETQ NEXT (CONS (LIST 'SETQ VALUEVAR (CADDDR X)) NEXT)))
               (T
                (SETQ NEXT
                        (CONS (LIST 'SETQ VALUEVAR (LIST 'CDR VALUEVAR))
                              NEXT))))))
            ((EQ (CAR CX) 'IN)
             (PROGN
              (SETQ VALUEVAR (GENSYM))
              (SETQ IV (CADR CX))
              (SETQ LVARS (CONS VALUEVAR (CONS IV LVARS)))
              (SETQ INIT (CONS (LIST 'SETQ VALUEVAR (CADDR CX)) INIT))
              (SETQ $COND (CONS (LIST 'NULL VALUEVAR) $COND))
              (SETQ CUR (CONS (LIST 'SETQ IV (LIST 'CAR VALUEVAR)) CUR))
              (COND
               ((CDDDR CX)
                (SETQ NEXT
                        (CONS (LIST 'SETQ VALUEVAR (LIST (CADDDR CX))) NEXT)))
               (T
                (SETQ NEXT
                        (CONS (LIST 'SETQ VALUEVAR (LIST 'CDR VALUEVAR))
                              NEXT))))))
            ((EQ (CAR CX) 'INCR)
             (PROG (INCR INCRVAR)
               (SETQ VALUEVAR (CADR CX))
               (SETQ CX (CDDR CX))
               (SETQ LVARS (CONS VALUEVAR LVARS))
               (SETQ INIT (CONS (LIST 'SETQ VALUEVAR (CAR CX)) INIT))
               (SETQ INCR (CADDR CX))
               (COND ((NUMBERP INCR) NIL)
                     ((AND (EQCAR INCR 'MINUS) (NUMBERP (CADR INCR)))
                      (SETQ INCR (MINUS (CADR INCR))))
                     (T
                      (PROGN
                       (SETQ INCRVAR (GENSYM))
                       (SETQ LVARS (CONS INCRVAR LVARS))
                       (SETQ INIT (CONS (LIST 'SETQ INCRVAR INCR) INIT))
                       (SETQ INCR INCRVAR))))
               (SETQ $COND
                       (CONS
                        (COND
                         (INCRVAR
                          (LIST 'COND
                                (LIST (LIST 'MINUSP INCR)
                                      (LIST 'LESSP VALUEVAR (CADR CX)))
                                (LIST 'T (LIST 'GREATERP VALUEVAR (CADR CX)))))
                         ((MINUSP INCR)
                          (COND (*FASTFOR (LIST 'ILESSP VALUEVAR (CADR CX)))
                                (T (LIST 'LESSP VALUEVAR (CADR CX)))))
                         (*FASTFOR (LIST 'IGREATERP VALUEVAR (CADR CX)))
                         (T (LIST 'GREATERP VALUEVAR (CADR CX))))
                        $COND))
               (SETQ NEXT
                       (CONS
                        (LIST 'SETQ VALUEVAR
                              (LIST
                               (COND ((OR INCRVAR (NOT *FASTFOR)) 'PLUS2)
                                     (T 'IPLUS2))
                               VALUEVAR INCR))
                        NEXT))))
            ((MEMQ (CAR CX)
                   '(SUM PRODUCT APPEND JOIN COUNT COLLECT MAXIMIZE MINIMIZE))
             (PROGN
              (SETQ CURVAR (GENSYM))
              (SETQ LVARS (CONS CURVAR LVARS))
              (COND
               ((EQ (CAR CX) 'PRODUCT)
                (SETQ INIT (ACONC* INIT (LIST 'SETQ CURVAR 1))))
               ((MEMQ (CAR CX) '(COUNT SUM))
                (SETQ INIT (ACONC* INIT (LIST 'SETQ CURVAR 0))))
               ((MEMQ (CAR CX) '(MAXIMIZE MINIMIZE))
                (PROGN
                 (SETQ *MAXMINFLAG T)
                 (COND
                  ((EQ ACTION 'IN)
                   (SETQ Y (LIST 'SETQ IV (LIST 'CAR VALUEVAR)))))
                 (COND
                  ((MEMQ ACTION '(IN ON))
                   (SETQ Y
                           (CONS
                            (LIST 'COND
                                  (LIST (LIST 'NULL VALUEVAR) '(RETURN 0)))
                            Y))))
                 (NCONC* INIT Y))))
              (COND
               ((EQ (CAR CX) 'COLLECT) (SETQ RETS (LIST 'REVERSIP CURVAR)))
               (T (SETQ RETS CURVAR)))
              (SETQ BODY
                      (CONS
                       (LIST 'SETQ CURVAR
                             (LIST (GET (CAR CX) 'BIN)
                                   (COND
                                    ((MEMQ (CAR CX) '(APPEND COUNT JOIN))
                                     CURVAR)
                                    (T (CDR CX)))
                                   (COND
                                    ((MEMQ (CAR CX) '(APPEND JOIN)) (CDR CX))
                                    ((EQ (CAR CX) 'COUNT)
                                     (LIST 'COND (LIST (CDR CX) 1) '(T 0)))
                                    (T CURVAR))))
                       BODY))))
            ((EQ (CAR CX) 'RETURNS) (SETQ RETS (CDR CX)))
            ((EQ (CAR CX) 'DO) (SETQ BODY (ACONC BODY (CDR CX))))
            ((EQ (CAR CX) 'WHEN)
             (COND
              ($WHEN (SYMERR "Redundant WHEN or UNLESS in FOR statement" NIL))
              (T (SETQ $WHEN (CDR CX)))))
            ((EQ (CAR CX) 'UNLESS)
             (COND
              ($WHEN (SYMERR "Redundant WHEN or UNLESS in FOR statement" NIL))
              (T (SETQ $WHEN (LIST 'NOT (CDR CX))))))
            ((EQ (CAR CX) 'UNTIL)
             (SETQ $WHILE (APPEND $WHILE (LIST (CDR CX)))))
            ((EQ (CAR CX) 'SOME)
             (SETQ CUR
                     (APPEND CUR
                             (LIST
                              (LIST 'COND (LIST (CDR CX) (LIST 'RETURN T)))))))
            ((EQ (CAR CX) 'EVERY)
             (PROGN
              (COND ((NOT RETS) (SETQ RETS T)))
              (SETQ CUR
                      (APPEND CUR
                              (LIST
                               (LIST 'COND
                                     (LIST (LIST 'NULL (CDR CX))
                                           (LIST 'RETURN NIL))))))))
            (T (REDERR (LIST (CAR CX) "invalid in FOR form"))))
      (SETQ X (CDR X))
      (GO LOOP))) 
(PUT 'FORCOND 'NUMBER-OF-ARGS 1) 
(PUT 'FORCOND 'DEFINED-ON-LINE '362) 
(PUT 'FORCOND 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'FORCOND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FORCOND (U)
    (LIST 'COND
          (LIST (CAR U) (COND ((CDDR U) (CONS 'PROGN (CDR U))) (T (CADR U)))))) 
(PUT 'FORPROG 'NUMBER-OF-ARGS 1) 
(PUT 'FORPROG 'DEFINED-ON-LINE '365) 
(PUT 'FORPROG 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'FORPROG 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FORPROG (U) (CONS 'PROG (FORNILCHK U))) 
(PUT 'FORNILCHK 'NUMBER-OF-ARGS 1) 
(PUT 'FORNILCHK 'DEFINED-ON-LINE '368) 
(PUT 'FORNILCHK 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'FORNILCHK 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FORNILCHK (U)
    (COND ((NULL U) NIL) ((NULL (CAR U)) (FORNILCHK (CDR U)))
          (T (CONS (CAR U) (FORNILCHK (CDR U)))))) 
(PUT 'MAX2* 'NUMBER-OF-ARGS 2) 
(PUT 'MAX2* 'DEFINED-ON-LINE '373) 
(PUT 'MAX2* 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'MAX2* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAX2* (U V) (COND ((NULL V) U) (T (MAX2 U V)))) 
(PUT 'MIN2* 'NUMBER-OF-ARGS 2) 
(PUT 'MIN2* 'DEFINED-ON-LINE '375) 
(PUT 'MIN2* 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'MIN2* 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MIN2* (U V) (COND ((NULL V) U) (T (MIN2 U V)))) 
(PUT 'NULL2ZERO 'NUMBER-OF-ARGS 1) 
(PUT 'NULL2ZERO 'DEFINED-ON-LINE '377) 
(PUT 'NULL2ZERO 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'NULL2ZERO 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NULL2ZERO (U) (COND ((NULL U) 0) (T U))) 
(PUT 'MKFN 'NUMBER-OF-ARGS 2) 
(PUT 'MKFN 'DEFINED-ON-LINE '379) 
(PUT 'MKFN 'DEFINED-IN-FILE 'RLISP88/FOR88.RED) 
(PUT 'MKFN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKFN (X FN)
    (COND ((ATOM X) X) ((GREATERP (LENGTH X) 1) (CONS FN X)) (T (CAR X)))) 
(ENDMODULE) 