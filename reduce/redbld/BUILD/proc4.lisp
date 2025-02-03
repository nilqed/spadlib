(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'PROC4)) 
(FLUID '(*SPECIFICATION_REDUCE *SPECIFICATION)) 
(FLUID '(*SPEC *NOINLINES)) 
(SWITCH (LIST 'SPEC)) 
(PUT 'SPEC 'SIMPFG '((T (SETQ *SPECIFICATION_REDUCE T) NIL))) 
(PUT 'N_FORMPROC 'NUMBER-OF-ARGS 2) 
(PUT 'N_FORMPROC 'DEFINED-ON-LINE '42) 
(PUT 'N_FORMPROC 'DEFINED-IN-FILE 'REDUCE4/PROC4.RED) 
(PUT 'N_FORMPROC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE N_FORMPROC (U VARS)
    (PROG (BODY NAME TRUENAME TYPE TYPELIST VARLIS X Y MODE CONSTRAINT
           PCKG_ORIG)
      (SETQ U (CDR U))
      (SETQ NAME (CAR U))
      (COND ((CADR U) (SETQ MODE (CADR U))) (T (SETQ MODE 'GENERIC)))
      (SETQ U (CDDR U))
      (SETQ TYPE (COND ((ATOM (CAR U)) (CAR U)) (T (CAAR U))))
      (SETQ PCKG_ORIG (COND ((ATOM (CAR U)) NIL) (T (CDAR U))))
      (COND
       ((AND (FLAGP NAME 'LOSE) (OR *LOSE (NULL *DEFN)))
        (RETURN (PROGN (LPRIM (LIST NAME "not defined (LOSE flag)")) NIL)))
       ((AND *REDEFLG* (GETD NAME)) (LPRIM (LIST NAME "redefined"))))
      (SETQ VARLIS (CADR U))
      (SETQ U (CADDR U))
      (SETQ X (COND ((EQCAR U 'BLOCK) (CADR U)) (T NIL)))
      (SETQ Y (APPEND VARLIS X))
      (SETQ TYPELIST
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J VARLIS)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARLIS
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J VARLIS)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CAR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ CONSTRAINT MODE)
      (SETQ TRUENAME NAME)
      (COND
       (*SPECIFICATION_REDUCE
        (COND ((SETQ NAME (GET_DISAMBOP NAME TYPELIST MODE)) NIL)
              (T (REDERR (LIST "no rank definition found for" NAME)))))
       (T
        (SETQ NAME
                (MKRANKEDNAME NAME TYPELIST
                 (COND ((ATOM MODE) NIL) (T (CAAR MODE)))))))
      (COND
       ((AND (NULL (ATOM MODE)) (CDR MODE))
        (PROGN
         (SETQ MODE (CAAR MODE))
         (SETQ CONSTRAINT
                 (LIST MODE
                       (MKQUOTE
                        (LIST 'LAMBDA VARLIS
                              (LIST 'COND
                                    (LIST (CAADR CONSTRAINT)
                                          (MKQUOTE NAME)))))))))
       (T (SETQ CONSTRAINT (LIST VARLIS (LIST T (MKQUOTE (LIST NAME MODE)))))))
      (SETQ BODY (N_FORM1 U Y))
      (COND
       ((AND (NOT (EQ MODE 'GENERIC)) *SPECIFICATION)
        (COND ((EQ (TYPE BODY) 'GENERIC) (SETQ BODY (VALUE BODY)))
              ((XTYPE1 (TYPE BODY) MODE) (SETQ BODY (VALUE BODY)))
              ((XTYPE1 MODE (TYPE BODY))
               (PROGN
                (LPRIM
                 (LIST "procedure definition is only valid if type of body"
                       (TYPE BODY) "is retractable to" MODE))
                (SETQ BODY (MKRETRACT (TYPE BODY) MODE (VALUE BODY)))))
              (T
               (REDERR
                (LIST "procedure type" MODE "is unrelated to ceiling type"
                      (TYPE BODY) "of procedure body")))))
       ((EQ MODE 'GENERIC) (SETQ BODY (VALUE BODY)))
       (T (SETQ BODY (LIST 'CHECK_TYPE (VALUE BODY) (MKQUOTE MODE)))))
      (COND ((AND *NOINLINES (EQ TYPE 'INLINE)) (SETQ TYPE 'EXPR)))
      (COND ((EQ TYPE 'EXPR) (SETQ BODY (LIST 'DE NAME VARLIS BODY)))
            ((EQ TYPE 'FEXPR) (SETQ BODY (LIST 'DF NAME VARLIS BODY)))
            ((EQ TYPE 'MACRO) (SETQ BODY (LIST 'DM NAME VARLIS BODY)))
            ((EQ TYPE 'EMB) (RETURN (EMBFN NAME VARLIS BODY)))
            (T
             (SETQ BODY
                     (LIST 'PUTC (MKQUOTE NAME) (MKQUOTE TYPE)
                           (MKQUOTE (LIST 'LAMBDA VARLIS BODY))))))
      (SETQ BODY
              (COND
               (*SPECIFICATION_REDUCE
                (LIST MODE
                      (LIST 'PROGN BODY
                            (MKQUOTE (MKOBJECT TRUENAME 'VARIABLE)))))
               (T
                (LIST MODE
                      (LIST 'PROGN
                            (LIST 'ADDRANK0 (MKQUOTE TRUENAME)
                                  (MKQUOTE TYPELIST) (MKQUOTE CONSTRAINT))
                            (LIST 'PUT (MKQUOTE NAME) ''PCKG_ORIG
                                  (MKQUOTE PCKG_ORIG))
                            BODY (MKQUOTE (MKOBJECT TRUENAME 'VARIABLE)))))))
      (COND
       ((AND *DEFN (MEMQ TYPE '(FEXPR MACRO INLINE SMACRO))) (LISPEVAL BODY)))
      (RETURN BODY))) 
(PUT 'MKRETRACT 'NUMBER-OF-ARGS 3) 
(PUT 'MKRETRACT 'DEFINED-ON-LINE '118) 
(PUT 'MKRETRACT 'DEFINED-IN-FILE 'REDUCE4/PROC4.RED) 
(PUT 'MKRETRACT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MKRETRACT (ATYP TTYP EXP) (LIST 'RETRACT (MKQUOTE ATYP) (MKQUOTE TTYP) EXP)) 
(PUT 'RETRACT 'NUMBER-OF-ARGS 3) 
(PUT 'RETRACT 'DEFINED-ON-LINE '121) 
(PUT 'RETRACT 'DEFINED-IN-FILE 'REDUCE4/PROC4.RED) 
(PUT 'RETRACT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE RETRACT (ATYP TTYP EXP)
    (COND ((XTYPE1 TTYP (TYPE EXP)) EXP) (T (REDERR "was not retractable")))) 
(PUT 'PROCEDURE 'N_FORMFN 'N_FORMPROC) 
(ENDMODULE) 