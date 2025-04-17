(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INTRO)) 
(FLUID
 '(*CREF *EXP *FACTOR *FORT *IFACTOR *INTSTR *LCM *MCD *MSG *MODE *NAT *NERO
   *PERIOD *PRECISE *PRI *PROTFG *RATIONALIZE *REDUCED *SUB2 *VAROPT *WSM POSN*
   SUBFG*)) 
(GLOBAL
 '(*RESUBS *VAL ERFG* EXLIST* INITL* NAT** OFL* SIMPCOUNT* SIMPLIMIT* TSTACK*)) 
(SETQ *EXP T) 
(SETQ *LCM T) 
(SETQ *MCD T) 
(SETQ *MODE (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 'SYMBOLIC)) 
(SETQ *MSG T) 
(PUT 'MSG 'SWITCHDEFAULT T) 
(SETQ *NAT T) 
(SETQ *PERIOD T) 
(SETQ *PRECISE T) 
(SETQ *RESUBS T) 
(SETQ *VAL T) 
(SETQ *VAROPT T) 
(SETQ EXLIST* '((*))) 
(SETQ INITL* (APPEND '(SUBFG* *SUB2 TSTACK*) INITL*)) 
(SETQ SIMPCOUNT* 0) 
(SETQ SIMPLIMIT* 2000) 
(SETQ SUBFG* T) 
(SETQ TSTACK* 0) 
(PUT 'SUBFG* 'INITL T) 
(PUT 'TSTACK* 'INITL 0) 
(PUT 'REVERSIP2 'NUMBER-OF-ARGS 2) 
(PUT 'REVERSIP2 'DEFINED-ON-LINE '132) 
(PUT 'REVERSIP2 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'REVERSIP2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REVERSIP2 (A B)
    (PROG (W)
      (PROG ()
       WHILELABEL
        (COND ((NOT A) (RETURN NIL)))
        (PROGN (SETQ W (CDR A)) (RPLACD A B) (SETQ B A) (SETQ A W))
        (GO WHILELABEL))
      (RETURN B))) 
(SETQ TRACELEXER T) 
(PUT 'MKID 'NUMBER-OF-ARGS 2) 
(PUT 'MKID 'DEFINED-ON-LINE '145) 
(PUT 'MKID 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'MKID 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MKID (X Y)
    (COND ((NOT (IDP X)) (TYPERR X "MKID root"))
          ((AND (ATOM Y) (OR (IDP Y) (AND (FIXP Y) (NOT (MINUSP Y)))))
           (INTERN
            (COMPRESS
             (NCONC (EXPLODE (GET-PRINT-NAME X))
                    (EXPLODE (GET-PRINT-NAME Y))))))
          (T (TYPERR Y "MKID index")))) 
(FLAG '(MKID) 'OPFN) 
(PUT 'MULTIPLE-RESULT 'NUMBER-OF-ARGS 2) 
(PUT 'MULTIPLE-RESULT 'DEFINED-ON-LINE '155) 
(PUT 'MULTIPLE-RESULT 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'MULTIPLE-RESULT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MULTIPLE-RESULT (Z W)
    (PROG (X Y)
      (COND ((NULL W) (RETURN (CONS 'LIST (REVERSIP* (FILLIN Z))))))
      (SETQ X (GETRTYPE W))
      (COND ((AND X (NOT (EQ X 'ARRAY))) (TYPERR W "array or id")))
      (LPRIW "*****"
             (LIST (COND ((EQ X 'ARRAY) "ARRAY") (T "ID"))
                   "fill no longer supported --- use lists instead"))
      (COND
       ((ATOM W)
        (COND
         ((NOT (ARRAYP W))
          (COND ((NUMBERP (SETQ W (REVAL1 W T))) (TYPERR W 'ID))))))
       ((NOT (ARRAYP (CAR W))) (TYPERR (CAR W) 'ARRAY))
       (T
        (SETQ W
                (CONS (CAR W)
                      (PROG (X FORALL-RESULT FORALL-ENDPTR)
                        (SETQ X (CDR W))
                        (COND ((NULL X) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (X)
                                            (COND ((EQ X 'TIMES) X)
                                                  (T (REVAL1 X T))))
                                          (CAR X))
                                         NIL)))
                       LOOPLABEL
                        (SETQ X (CDR X))
                        (COND ((NULL X) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (X)
                                    (COND ((EQ X 'TIMES) X) (T (REVAL1 X T))))
                                  (CAR X))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))))
      (SETQ X (DIFFERENCE (LENGTH Z) 1))
      (COND
       ((NOT
         (OR (AND (NOT (ATOM W)) (ATOM (CAR W)) (SETQ Y (DIMENSION (CAR W))))
             (AND (SETQ Y (DIMENSION W)) (NULL (CDR Y)))))
        (PROGN
         (SETQ Y (EXPLODE W))
         (SETQ W NIL)
         (PROG (J)
           (SETQ J Z)
          LAB
           (COND ((NULL J) (RETURN NIL)))
           ((LAMBDA (J)
              (PROGN
               (SETQ W
                       (CONS (INTERN (COMPRESS (APPEND Y (EXPLODE (CAR J)))))
                             W))
               (SETK1 (CAR W) (CDR J) T)))
            (CAR J))
           (SETQ J (CDR J))
           (GO LAB))
         (LPRIM
          (COND ((EQUAL (LENGTH W) 1) (LIST (CAR W) "is non zero"))
                (T (ACONC* (REVERSIP* W) "are non zero"))))
         (RETURN X)))
       ((ATOM W)
        (PROGN
         (COND
          ((NEQ (CAAR Z) (DIFFERENCE (CAR Y) 1))
           (PROGN
            (SETQ Y (LIST (PLUS (CAAR Z) 1)))
            (PUT W 'AVALUE (LIST 'ARRAY (MKARRAY1 Y 'ALGEBRAIC)))
            (PUT W 'DIMENSION Y))))
         (SETQ W (LIST W 'TIMES)))))
      (SETQ Y (PAIR (CDR W) Y))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND Y (NOT (SMEMQ 'TIMES (CAAR Y))))) (RETURN NIL)))
        (SETQ Y (CDR Y))
        (GO WHILELABEL))
      (COND ((NULL Y) (ERRACH "MULTIPLE-RESULT")))
      (SETQ Y
              (DIFFERENCE
               (DIFFERENCE (CDAR Y) (REVAL1 (SUBST 0 'TIMES (CAAR Y)) T)) 1))
      (COND
       ((GREATERP (CAAR Z) Y)
        (RERROR 'ALG 3 (LIST "Index" (CAAR Z) "out of range"))))
      (PROG ()
       REPEATLABEL
        (COND ((OR (NULL Z) (NEQ Y (CAAR Z))) (SETELV (SUBST Y 'TIMES W) 0))
              (T
               (PROGN (SETELV (SUBST Y 'TIMES W) (CDAR Z)) (SETQ Z (CDR Z)))))
        (COND ((NOT (LESSP (SETQ Y (DIFFERENCE Y 1)) 0)) (GO REPEATLABEL))))
      (RETURN X))) 
(PUT 'FILLIN 'NUMBER-OF-ARGS 1) 
(PUT 'FILLIN 'DEFINED-ON-LINE '208) 
(PUT 'FILLIN 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'FILLIN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FILLIN (U) (COND ((NULL U) NIL) (T (FILLIN1 U (CAAR U))))) 
(PUT 'FILLIN1 'NUMBER-OF-ARGS 2) 
(PUT 'FILLIN1 'DEFINED-ON-LINE '213) 
(PUT 'FILLIN1 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'FILLIN1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FILLIN1 (U N)
    (COND ((LESSP N 0) NIL)
          ((AND U (EQUAL (CAAR U) N))
           (CONS (CDAR U) (FILLIN1 (CDR U) (DIFFERENCE N 1))))
          (T (CONS 0 (FILLIN1 U (DIFFERENCE N 1)))))) 
(PUT 'MSGPRI 'NUMBER-OF-ARGS 5) 
(PUT 'MSGPRI 'DEFINED-ON-LINE '221) 
(PUT 'MSGPRI 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'MSGPRI 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE MSGPRI (U V W X Y)
    (PROG (POSN* NAT1 Z PLINE*)
      (SETQ POSN* 0)
      (COND ((AND (NULL Y) (NULL *MSG)) (RETURN NIL)))
      (SETQ NAT1 *NAT)
      (SETQ *NAT NIL)
      (COND ((AND OFL* (OR *FORT (NOT NAT1))) (GO C)))
     A
      (TERPRI)
      (LPRI
       (CONS (COND ((NULL Y) "***") (T "*****"))
             (COND ((AND U (ATOM U)) (LIST U)) (T U))))
      (SETQ POSN* (POSN))
      (MAPRIN V)
      (PRIN2 " ")
      (LPRI (COND ((AND W (ATOM W)) (LIST W)) (T W)))
      (SETQ POSN* (POSN))
      (MAPRIN X)
      (TERPRI* T)
      (COND ((NULL Z) (GO B)))
      (WRS (CDR Z))
      (GO D)
     B
      (COND ((NULL OFL*) (GO D)))
     C
      (SETQ Z OFL*)
      (WRS NIL)
      (GO A)
     D
      (SETQ *NAT NAT1)
      (COND (Y (COND ((EQ Y 'HOLD) (SETQ ERFG* Y)) (T (ERROR1))))))) 
(PUT 'ERRACH 'NUMBER-OF-ARGS 1) 
(PUT 'ERRACH 'DEFINED-ON-LINE '248) 
(PUT 'ERRACH 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'ERRACH 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERRACH (U)
    (PROG ()
      (TERPRI* T)
      (LPRIE "CATASTROPHIC ERROR *****")
      (PRINTTY U)
      (LPRIW " " NIL)
      (RERROR 'ALG 4
              "Please report output and input listing on the sourceforge bug tracker"))) 
(PUT 'ERRPRI1 'NUMBER-OF-ARGS 1) 
(PUT 'ERRPRI1 'DEFINED-ON-LINE '258) 
(PUT 'ERRPRI1 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'ERRPRI1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ERRPRI1 (U) (MSGPRI "Substitution for" U "not allowed" NIL T)) 
(PUT 'ERRPRI2 'NUMBER-OF-ARGS 2) 
(PUT 'ERRPRI2 'DEFINED-ON-LINE '261) 
(PUT 'ERRPRI2 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'ERRPRI2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ERRPRI2 (U V) (MSGPRI "Syntax error:" U "invalid" NIL V)) 
(PUT 'REDMSG 'NUMBER-OF-ARGS 2) 
(PUT 'REDMSG 'DEFINED-ON-LINE '264) 
(PUT 'REDMSG 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'REDMSG 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE REDMSG (U V)
    (COND
     ((AND (NULL *WSM) *MSG (MEMBER V '("operator" "predicate")))
      (COND
       ((TERMINALP)
        (OR (YESP (LIST "Declare" (GET-PRINT-NAME U) V "?")) (ERROR1)))
       (T (LPRIM (LIST (GET-PRINT-NAME U) "declared" V))))))) 
(PUT 'TYPERR 'NUMBER-OF-ARGS 2) 
(PUT 'TYPERR 'DEFINED-ON-LINE '271) 
(PUT 'TYPERR 'DEFINED-IN-FILE 'ALG/INTRO.RED) 
(PUT 'TYPERR 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TYPERR (U V)
    (COND
     (OUTPUTHANDLER*
      (RERROR 'RLISP 6
              (COND
               ((AND (NOT (ATOM U)) (ATOM (CAR U)) (CDR U) (ATOM (CADR U))
                     (NULL (CDDR U)))
                (LIST (CAR U) (CADR U) "invalid as" V))
               (T (LIST U "invalid as" V)))))
     (T
      (PROGN
       (COND
        ((NOT *PROTFG)
         (PROGN
          (TERPRI* T)
          (PRIN2* "***** ")
          (COND
           ((AND (NOT (ATOM U)) (ATOM (CAR U)) (CDR U) (ATOM (CADR U))
                 (NULL (CDDR U)))
            (PROGN (PRIN2* (CAR U)) (PRIN2* " ") (PRIN2* (CADR U))))
           ((NULL U) (PRIN2* U)) (T (MAPRIN U)))
          (PRIN2* " invalid as ")
          (PRIN2* V)
          (TERPRI* NIL))))
       (SETQ ERRMSG* (LIST U "invalid as" V))
       (SETQ ERFG* T)
       (ERROR1))))) 
(FLAG
 '(AEVAL COND GETEL GO PROG PROGN PROG2 RETURN REVAL REVAL1 SETQ SETK SETEL
         ASSGNPRI *S2I)
 'NOCHANGE) 
(FLAG
 '(OR AND NOT MEMBER MEMQ EQUAL NEQ EQ GEQ GREATERP LEQ FIXP LESSP NUMBERP ORDP
      FREEOF)
 'BOOLEAN) 
(FLAG '(OR AND NOT) 'BOOLARGS) 
(DEFLIST
 '((EXP ((NIL (RMSUBS)) (T (RMSUBS))))
   (FACTOR ((NIL (SETQ *EXP T) (RMSUBS)) (T (SETQ *EXP NIL) (RMSUBS))))
   (FORT ((NIL (SETQ *NAT NAT**)) (T (SETQ *NAT NIL)))) (GCD ((T (RMSUBS))))
   (INTSTR ((NIL (RMSUBS)) (T (RMSUBS)))) (MCD ((NIL (RMSUBS)) (T (RMSUBS))))
   (NAT ((NIL (SETQ NAT** NIL)) (T (SETQ NAT** T)))) (NUMVAL ((T (RMSUBS))))
   (RATIONALIZE ((T (RMSUBS)))) (REDUCED ((T (RMSUBS)))) (VAL ((T (RMSUBS)))))
 'SIMPFG) 
(SWITCH
 (LIST (LIST 'EQUAL 'EXP 'ON) 'CREF 'FACTOR 'FORT 'GCD 'IFACTOR 'INTSTR
       (LIST 'EQUAL 'LCM 'ON) (LIST 'EQUAL 'MCD 'ON) (LIST 'EQUAL 'NAT 'ON)
       'NERO (LIST 'EQUAL 'NUMVAL 'ON) (LIST 'EQUAL 'PERIOD 'ON)
       (LIST 'EQUAL 'PRECISE 'ON) (LIST 'EQUAL 'PRI 'ON) 'RATIONALIZE 'REDUCED
       (LIST 'EQUAL 'VAROPT 'ON))) 
(ENDMODULE) 