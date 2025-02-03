(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CSOLVE)) 
(FLUID '(*TRINT CCOUNT CMAP CMATRIX CVAL LOGLIST NEWEQN)) 
(EXPORTS
 (LIST 'BACKSUBST4CS 'CREATECMAP 'FINDPIVOT 'PRINTVECSQ 'SPREADC
       'SUBST4ELIMINATEDS)) 
(IMPORTS
 (LIST 'NTH 'INTERR '*MULTF 'PRINTSF 'PRINTSQ 'QUOTF 'PUTV 'NEGF 'INVSQ 'NEGSQ
       'ADDSQ 'MULTSQ 'MKSP 'ADDF 'DOMAINP 'PNTH)) 
(PUT 'FINDPIVOT 'NUMBER-OF-ARGS 1) 
(PUT 'FINDPIVOT 'DEFINED-ON-LINE '38) 
(PUT 'FINDPIVOT 'DEFINED-IN-FILE 'INT/CSOLVE.RED) 
(PUT 'FINDPIVOT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FINDPIVOT (CVEC)
    (PROG (I X)
      (SETQ I 1)
      (SETQ X (GETV CVEC I))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (LESSP I CCOUNT) (NULL X))) (RETURN NIL)))
        (PROGN (SETQ I (PLUS I 1)) (SETQ X (GETV CVEC I)))
        (GO WHILELABEL))
      (COND ((NULL X) (RETURN NIL)))
      (RETURN I))) 
(PUT 'SUBST4ELIMINATEDCS 'NUMBER-OF-ARGS 3) 
(PUT 'SUBST4ELIMINATEDCS 'DEFINED-ON-LINE '51) 
(PUT 'SUBST4ELIMINATEDCS 'DEFINED-IN-FILE 'INT/CSOLVE.RED) 
(PUT 'SUBST4ELIMINATEDCS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBST4ELIMINATEDCS (NEWEQN SUBSTORDER CEQNS)
    (COND ((NULL SUBSTORDER) NEWEQN)
          (T
           (PROG (NXT ROW CVAR TEMP)
             (SETQ ROW (CAR CEQNS))
             (SETQ NXT (CAR SUBSTORDER))
             (COND
              ((NULL (SETQ CVAR (GETV NEWEQN NXT)))
               (RETURN
                (SUBST4ELIMINATEDCS NEWEQN (CDR SUBSTORDER) (CDR CEQNS)))))
             (SETQ NXT (GETV ROW NXT))
             (PROG (I)
               (SETQ I 0)
              LAB
               (COND ((MINUSP (DIFFERENCE CCOUNT I)) (RETURN NIL)))
               (PROGN
                (SETQ TEMP (*MULTF NXT (GETV NEWEQN I)))
                (SETQ TEMP (ADDF TEMP (NEGF (*MULTF CVAR (GETV ROW I)))))
                (PUTV NEWEQN I TEMP))
               (SETQ I (PLUS2 I 1))
               (GO LAB))
             (RETURN
              (SUBST4ELIMINATEDCS NEWEQN (CDR SUBSTORDER) (CDR CEQNS))))))) 
(PUT 'BACKSUBST4CS 'NUMBER-OF-ARGS 3) 
(PUT 'BACKSUBST4CS 'DEFINED-ON-LINE '71) 
(PUT 'BACKSUBST4CS 'DEFINED-IN-FILE 'INT/CSOLVE.RED) 
(PUT 'BACKSUBST4CS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE BACKSUBST4CS (CS2SUBST CS2SOLVE CMATRIX)
    (COND ((NULL CMATRIX) NIL)
          (T
           (PROG (EQNN CVAR ALREADY SUBSTLIST TEMP TEMP2)
             (SETQ EQNN (CAR CMATRIX))
             (SETQ CVAR (CAR CS2SOLVE))
             (SETQ ALREADY (CONS NIL 1))
             (SETQ SUBSTLIST CS2SUBST)
             (PROG ()
              WHILELABEL
               (COND ((NOT (NOT (NULL SUBSTLIST))) (RETURN NIL)))
               (PROGN
                (SETQ TEMP (CAR SUBSTLIST))
                (COND
                 ((NOT (NULL (GETV EQNN TEMP)))
                  (SETQ ALREADY
                          (ADDSQ ALREADY
                                 (MULTSQ (CONS (GETV EQNN TEMP) 1)
                                         (GETV CVAL TEMP))))))
                (SETQ SUBSTLIST (CDR SUBSTLIST)))
               (GO WHILELABEL))
             (SETQ TEMP (NEGSQ (ADDSQ (CONS (GETV EQNN 0) 1) ALREADY)))
             (COND
              ((NOT
                (NULL
                 (SETQ TEMP2
                         ((LAMBDA (*EXP) (QUOTF1 (CAR TEMP) (GETV EQNN CVAR)))
                          T))))
               (SETQ TEMP (CONS TEMP2 (CDR TEMP))))
              (T (SETQ TEMP (MULTSQ TEMP (INVSQ (CONS (GETV EQNN CVAR) 1))))))
             (COND
              ((NOT (NULL (CAR TEMP)))
               (PUTV CVAL CVAR (RESIMP (ROOTEXTRACTSQ (SUBS2Q TEMP))))))
             (BACKSUBST4CS (REVERSIP (CONS CVAR (REVERSIP CS2SUBST)))
              (CDR CS2SOLVE) (CDR CMATRIX)))))) 
(PUT 'CREATECMAP 'NUMBER-OF-ARGS 0) 
(PUT 'CREATECMAP 'DEFINED-ON-LINE '104) 
(PUT 'CREATECMAP 'DEFINED-IN-FILE 'INT/CSOLVE.RED) 
(PUT 'CREATECMAP 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE CREATECMAP NIL
    (PROG (I L C)
      (SETQ L LOGLIST)
      (SETQ I 1)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NOT (NULL L))) (RETURN NIL)))
        (PROGN
         (SETQ C (CONS (CONS (INT-GENSYM1 'C) I) C))
         (SETQ I (PLUS I 1))
         (RPLACD (CAR L)
                 (CONS (CONS (CONS (GETPOWER (FKERN (CAAR C)) 1) 1) NIL)
                       (CDAR L)))
         (SETQ L (CDR L)))
        (GO WHILELABEL))
      (COND
       (*TRINT
        ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X))
         (CONS "Constants Created for log and tan terms:" C))))
      (RETURN C))) 
(PUT 'SPREADC 'NUMBER-OF-ARGS 3) 
(PUT 'SPREADC 'DEFINED-ON-LINE '123) 
(PUT 'SPREADC 'DEFINED-IN-FILE 'INT/CSOLVE.RED) 
(PUT 'SPREADC 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SPREADC (EQNN CVEC1 W)
    (COND
     ((OR (ATOM EQNN) (ATOM (CAR EQNN)))
      (PUTV CVEC1 0 (ADDF (GETV CVEC1 0) (*MULTF EQNN W))))
     (T
      (PROG (MV T1 T2)
        (SPREADC (CDR EQNN) CVEC1 W)
        (SETQ MV (CAAAR EQNN))
        (SETQ T1 (ASSOC MV CMAP))
        (COND
         ((NOT (NULL T1))
          (RETURN
           (PROGN
            (SETQ T1 (CDR T1))
            (COND
             ((NOT (EQUAL (CDAR (CAR EQNN)) 1))
              (INTERR "Not linear in c eqn")))
            (SETQ T2 (ADDF (GETV CVEC1 T1) (*MULTF W (CDAR EQNN))))
            (PUTV CVEC1 T1 T2)))))
        (SETQ T1 (CONS (CONS (CAAR EQNN) 1) NIL))
        (SPREADC (CDAR EQNN) CVEC1 (*MULTF W T1)))))) 
(ENDMODULE) 