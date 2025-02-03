(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'ISOLVE)) 
(FLUID
 '(*TRINT BADPART CCOUNT CMAP CMATRIX CVAL INDEXLIST LHS* LORDER ORDEROFELIM
   POWER-LIST* PT RHS* SILLIESLIST TANLIST ULIST ZLIST)) 
(GLOBAL '(*NUMBER* *STATISTICS)) 
(EXPORTS (LIST 'SOLVE-FOR-U)) 
(IMPORTS
 (LIST 'NTH 'FINDPIVOT 'GCDF 'INT-GENSYM1 'MKVECT 'INTERR 'MULTDFCONST '*MULTF*
       'NEGDF 'ORDDF 'PLUSDF 'PRINTDF 'PRINTSF 'PRINTSPREADC 'PRINTSQ 'QUOTF
       'PUTV 'SPREADC 'SUBST4ELIMINATEDCS 'MKNILL 'PNTH 'DOMAINP 'ADDF 'INVSQ
       'MULTSQ)) 
(PUT 'UTERM 'NUMBER-OF-ARGS 2) 
(PUT 'UTERM 'DEFINED-ON-LINE '58) 
(PUT 'UTERM 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'UTERM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE UTERM (POWU RHS*)
    (COND ((NULL RHS*) NIL)
          (T
           (PROG (COEF POWER)
             (SETQ POWER (ADDINDS POWU (CAAR RHS*)))
             (SETQ COEF (EVALUATECOEFFTS (CAR (CDAR RHS*)) POWU))
             (COND ((NULL COEF) (RETURN (UTERM POWU (CDR RHS*)))))
             (SETQ COEF (CONS COEF (CDR (CDAR RHS*))))
             (RETURN
              (PLUSDF (CONS (CONS POWER COEF) NIL) (UTERM POWU (CDR RHS*)))))))) 
(PUT 'SOLVE-FOR-U 'NUMBER-OF-ARGS 3) 
(PUT 'SOLVE-FOR-U 'DEFINED-ON-LINE '70) 
(PUT 'SOLVE-FOR-U 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'SOLVE-FOR-U 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SOLVE-FOR-U (RHS* LHS* ULIST)
    (PROG ()
     TOP
      (COND ((NULL LHS*) (RETURN ULIST))
            (T
             (PROG (U LPOWLHS)
               (SETQ LPOWLHS (CAAR LHS*))
               (PROG (LL M1 CHGE)
                 (SETQ LL (MAXORDER POWER-LIST* ZLIST 0))
                 (SETQ M1 LORDER)
                 (PROG ()
                  WHILELABEL
                   (COND ((NOT M1) (RETURN NIL)))
                   (PROGN
                    (COND
                     ((LESSP (CAR LL) (CAR M1))
                      (PROGN (SETQ CHGE T) (RPLACA M1 (CAR LL)))))
                    (SETQ LL (CDR LL))
                    (SETQ M1 (CDR M1)))
                   (GO WHILELABEL))
                 (COND
                  ((AND *TRINT CHGE)
                   (PROGN
                    (PRINC
                     "Maximum order for undetermined coefficients is reduced to ")
                    ((LAMBDA (X) (PROGN (PRIN2 X) (TERPRI) X)) LORDER)))))
               (SETQ U (PICKUPU RHS* (CAAR LHS*) T))
               (COND
                ((NULL U)
                 (PROGN
                  (COND
                   (*TRINT
                    (PROGN
                     (PROGN
                      (PRIN2 "***** Equation for a constant to be solved:")
                      (TERPRI)
                      "***** Equation for a constant to be solved:")
                     (PRINTSF (CAR (CDAR LHS*)))
                     (PROGN (PRIN2 "    = 0") (TERPRI) "    = 0")
                     (PROGN (PRIN2 " ") (TERPRI) " "))))
                  (COND
                   ((GAUSSELIMN (CAR (CDAR LHS*)) (CAR LHS*))
                    (PROGN
                     (SETQ LHS* (SQUASHCONSTANTS (CDR LHS*)))
                     (SETQ U T)))
                   (T (SETQ LHS* (CDR LHS*))))))
                (T
                 (PROGN
                  (SETQ ULIST
                          (CONS
                           (CONS (CAR U)
                                 (SUBS2Q
                                  (MULTSQ (COEFDF LHS* LPOWLHS)
                                          (INVSQ (CDR U)))))
                           ULIST))
                  (COND (*STATISTICS (SETQ *NUMBER* (PLUS *NUMBER* 1))))
                  (COND
                   (*TRINT
                    (PROGN
                     (PROGN
                      (PRIN2 "A coefficient of numerator has been determined")
                      (TERPRI)
                      "A coefficient of numerator has been determined")
                     (PRIN2 "***** U")
                     (PRIN2 (CAR U))
                     (PRIN2T " =")
                     (PRINTSQ (MULTSQ (COEFDF LHS* LPOWLHS) (INVSQ (CDR U))))
                     (PROGN (PRIN2 " ") (TERPRI) " "))))
                  (SETQ LHS*
                          (PLUSDF LHS*
                           (NEGDF
                            (MULTDFCONST (CDAR ULIST)
                             (UTERM (CAR U) RHS*))))))))
               (COND
                ((AND *TRINT U)
                 (PROGN
                  (PROGN
                   (PRIN2 "Terms remaining are:")
                   (TERPRI)
                   "Terms remaining are:")
                  (PRINTDF LHS*)
                  (PROGN (PRIN2 " ") (TERPRI) " ")))))))
      (GO TOP))) 
(PUT 'SQUASHCONSTANTS 'NUMBER-OF-ARGS 1) 
(PUT 'SQUASHCONSTANTS 'DEFINED-ON-LINE '122) 
(PUT 'SQUASHCONSTANTS 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'SQUASHCONSTANTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SQUASHCONSTANTS (EXPRESS)
    (COND ((NULL CMATRIX) EXPRESS)
          (T
           (PROG (CONSTLST II XP CL SUBBY CMT XX)
             (SETQ CONSTLST (REVERSE CMAP))
             (SETQ CMT CMATRIX)
            XXX
             (COND ((NULL CMT) (RETURN EXPRESS)))
             (SETQ XX (CAR CMT))
             (SETQ CL CONSTLST)
             (SETQ II 1)
             (PROG ()
              WHILELABEL
               (COND ((NOT (NOT (GETV XX II))) (RETURN NIL)))
               (PROGN (SETQ II (PLUS II 1)) (SETQ CL (CDR CL)))
               (GO WHILELABEL))
             (SETQ SUBBY (CAAR CL))
             (COND
              ((MEMBER SUBBY SILLIESLIST)
               (PROGN (SETQ CMT (CDR CMT)) (GO XXX))))
             (SETQ XP (PREPSQ (CONS (GETV XX 0) 1)))
             (SETQ CL (CDR CL))
             (COND
              ((NOT (EQUAL CCOUNT II))
               (PROG (JJ)
                 (SETQ JJ (PLUS II 1))
                LAB
                 (COND ((MINUSP (DIFFERENCE CCOUNT JJ)) (RETURN NIL)))
                 (PROGN
                  (COND
                   ((GETV XX JJ)
                    (SETQ XP
                            (LIST 'PLUS XP
                                  (LIST 'TIMES (CAAR CL)
                                        (PREPSQ (CONS (GETV XX JJ) 1)))))))
                  (SETQ CL (CDR CL)))
                 (SETQ JJ (PLUS2 JJ 1))
                 (GO LAB))))
             (SETQ XP
                     (LIST 'QUOTIENT (LIST 'MINUS XP)
                           (PREPSQ (CONS (GETV XX II) 1))))
             (COND
              (*TRINT
               (PROGN
                (PRIN2 "Replace constant ")
                (PRIN2 SUBBY)
                (PRIN2 " by ")
                (PRINTSQ (SIMP XP)))))
             (SETQ SILLIESLIST (CONS SUBBY SILLIESLIST))
             (RETURN (SUBDF EXPRESS XP SUBBY)))))) 
(PUT 'CHECKU 'NUMBER-OF-ARGS 2) 
(PUT 'CHECKU 'DEFINED-ON-LINE '154) 
(PUT 'CHECKU 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'CHECKU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECKU (ULST U)
    (AND ULST (OR (EQUAL (CAR U) (CAAR ULST)) (CHECKU (CDR ULST) U)))) 
(PUT 'CHECKU1 'NUMBER-OF-ARGS 2) 
(PUT 'CHECKU1 'DEFINED-ON-LINE '159) 
(PUT 'CHECKU1 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'CHECKU1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECKU1 (POWU RHS*)
    (PROG ()
     TOP
      (COND ((NULL RHS*) (RETURN NIL)))
      (COND
       ((NEGIND POWU (CAAR RHS*))
        (COND
         ((NOT (NULL (EVALUATECOEFFTS (CAR (CDAR RHS*)) POWU))) (RETURN T)))))
      (SETQ RHS* (CDR RHS*))
      (GO TOP))) 
(PUT 'NEGIND 'NUMBER-OF-ARGS 2) 
(PUT 'NEGIND 'DEFINED-ON-LINE '171) 
(PUT 'NEGIND 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'NEGIND 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NEGIND (PU PR)
    (AND PU
         (OR (LESSP (PLUS (CAR PU) (CAAR PR)) 0) (NEGIND (CDR PU) (CDR PR))))) 
(PUT 'EVALUATECOEFFTS 'NUMBER-OF-ARGS 2) 
(PUT 'EVALUATECOEFFTS 'DEFINED-ON-LINE '176) 
(PUT 'EVALUATECOEFFTS 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'EVALUATECOEFFTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALUATECOEFFTS (COEFFT INDLIST)
    (COND
     ((OR (NULL COEFFT) (OR (ATOM COEFFT) (ATOM (CAR COEFFT))))
      (COND ((EQUAL COEFFT 0) NIL) (T COEFFT)))
     (T
      (PROG (TEMP)
        (COND
         ((MEMBER (CAAAR COEFFT) INDEXLIST)
          (SETQ TEMP (VALUECOEFFT (CAAAR COEFFT) INDLIST INDEXLIST)))
         (T (SETQ TEMP (LIST (CONS (CAAR COEFFT) 1)))))
        (SETQ TEMP (*MULTF TEMP (EVALUATECOEFFTS (CDAR COEFFT) INDLIST)))
        (RETURN (ADDF TEMP (EVALUATECOEFFTS (CDR COEFFT) INDLIST))))))) 
(PUT 'VALUECOEFFT 'NUMBER-OF-ARGS 3) 
(PUT 'VALUECOEFFT 'DEFINED-ON-LINE '189) 
(PUT 'VALUECOEFFT 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'VALUECOEFFT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE VALUECOEFFT (VAR INDVALUES INDLIST)
    (COND ((NULL INDLIST) (INTERR "Valuecoefft - no value"))
          ((EQ VAR (CAR INDLIST))
           (COND ((EQUAL (CAR INDVALUES) 0) NIL) (T (CAR INDVALUES))))
          (T (VALUECOEFFT VAR (CDR INDVALUES) (CDR INDLIST))))) 
(PUT 'ADDINDS 'NUMBER-OF-ARGS 2) 
(PUT 'ADDINDS 'DEFINED-ON-LINE '198) 
(PUT 'ADDINDS 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'ADDINDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ADDINDS (POWU POWRHS)
    (COND
     ((NULL POWU) (COND ((NULL POWRHS) NIL) (T (INTERR "Powrhs too long"))))
     ((NULL POWRHS) (INTERR "Powu too long"))
     (T
      (CONS (PLUS (CAR POWU) (CAAR POWRHS))
            (ADDINDS (CDR POWU) (CDR POWRHS)))))) 
(PUT 'PICKUPU 'NUMBER-OF-ARGS 3) 
(PUT 'PICKUPU 'DEFINED-ON-LINE '206) 
(PUT 'PICKUPU 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'PICKUPU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE PICKUPU (RHS* POWLHS FLG)
    (PROG (COEFFU U)
      (SETQ PT RHS*)
     TOP
      (COND ((NULL PT) (RETURN NIL)))
      (SETQ U (NEXTU (CAR PT) POWLHS))
      (COND ((NULL U) (GO NOTTHISONE)))
      (COND ((NOT (TESTORD (CAR U) LORDER)) (GO NEVERTHISONE)))
      (COND ((NOT (CHECKCOEFFTS COEFFU (CAR U))) (GO NOTTHISONE)))
      (COND ((CHECKU ULIST U) (GO NOTTHISONE)))
      (COND ((CHECKU1 (CAR U) RHS*) (GO NEVERTHISONE)))
      (COND (FLG (SETQ U (PATCHUPTAN (LIST U) POWLHS (CDR PT) RHS*))))
      (RETURN U)
     NEVERTHISONE
      (SETQ COEFFU (CONS (CDAR PT) COEFFU))
     NOTTHISONE
      (SETQ PT (CDR PT))
      (GO TOP))) 
(PUT 'PATCHUPTAN 'NUMBER-OF-ARGS 4) 
(PUT 'PATCHUPTAN 'DEFINED-ON-LINE '235) 
(PUT 'PATCHUPTAN 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'PATCHUPTAN 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE PATCHUPTAN (U POWLHS RPT RHS*)
    (PROG (UU CC DD TANLIST REDU REDU1 MESGIVEN NEEDSQUASH)
      (SETQ PT RPT)
      (PROG ()
       WHILELABEL
        (COND ((NOT PT) (RETURN NIL)))
        (PROGN
         (COND
          ((AND (SETQ UU (PICKUPU PT POWLHS NIL)) (TESTORD (CAR UU) LORDER))
           (PROGN
            (SETQ CC (CONS (CONS (INT-GENSYM1 'C) (CAAR U)) CC))
            (COND
             (*TRINT
              (PROGN
               (COND
                ((NOT MESGIVEN)
                 (PROGN
                  (PRIN2T "*** Introduce new constants for coefficients")
                  (SETQ MESGIVEN T))))
               (PRIN2 "***** U")
               (PRIN2 (CAAR U))
               (PRIN2T " =")
               (PRINT (CAAR CC)))))
            (SETQ REDU
                    (PLUSDF REDU
                     (MULTDFCONST (CONS (LIST (CONS (CONS (CAAR CC) 1) 1)) 1)
                      (UTERM (CAAR U) RHS*))))
            (SETQ U (CONS UU U)))))
         (COND (PT (SETQ PT (CDR PT)))))
        (GO WHILELABEL))
      (SETQ REDU1 REDU)
      (PROG ()
       WHILELABEL
        (COND ((NOT REDU1) (RETURN NIL)))
        (PROG (XX)
          (SETQ XX (CAR REDU1))
          (COND
           (*TRINT
            (PROGN
             (PRIN2 "Introduced terms: ")
             (PRIN2 (CAR XX))
             (PRINC "*(")
             (PRINTSQ (CDR XX))
             (PROGN (PRIN2 ")") (TERPRI) ")"))))
          (COND
           ((NOT (TESTORD (CAR XX) LORDER))
            (PROGN
             (COND (*TRINT (PROGN (PRIN2 "  =  0") (TERPRI) "  =  0")))
             (COND
              ((SETQ DD (KILLSINGLES (CADR XX) CC))
               (PROGN
                (SETQ REDU (SUBDF REDU 0 (CAR DD)))
                (SETQ REDU1 (SUBDF REDU1 0 (CAR DD)))
                (SETQ ULIST (CONS (CONS (CDR DD) (CONS NIL 1)) ULIST))
                (SETQ U (RMVE U (CDR DD)))
                (SETQ CC (PURGECONST CC DD))))
              (T (PROGN (SETQ NEEDSQUASH T) (SETQ REDU1 (CDR REDU1)))))))
           (T (SETQ REDU1 (CDR REDU1)))))
        (GO WHILELABEL))
      (PROG (XX)
        (SETQ XX REDU)
       LAB
        (COND ((NULL XX) (RETURN NIL)))
        ((LAMBDA (XX)
           (PROGN
            (COND
             ((NOT (TESTORD (CAR XX) LORDER))
              (PROGN
               (PROG ()
                WHILELABEL
                 (COND ((NOT CC) (RETURN NIL)))
                 (PROGN
                  (ADDCTOMAP (CAAR CC))
                  (SETQ ULIST
                          (CONS
                           (CONS (CDAR CC)
                                 (CONS (LIST (CONS (CONS (CAAR CC) 1) 1)) 1))
                           ULIST))
                  (COND (*STATISTICS (SETQ *NUMBER* (PLUS *NUMBER* 1))))
                  (SETQ CC (CDR CC)))
                 (GO WHILELABEL))
               (GAUSSELIMN (CAR (CDAR REDU)) (CAR REDU)))))))
         (CAR XX))
        (SETQ XX (CDR XX))
        (GO LAB))
      (COND
       (REDU
        (PROGN
         (PROG ()
          WHILELABEL
           (COND ((NOT CC) (RETURN NIL)))
           (PROGN
            (ADDCTOMAP (CAAR CC))
            (SETQ ULIST
                    (CONS
                     (CONS (CDAR CC)
                           (CONS (LIST (CONS (CONS (CAAR CC) 1) 1)) 1))
                     ULIST))
            (COND (*STATISTICS (SETQ *NUMBER* (PLUS *NUMBER* 1))))
            (SETQ CC (CDR CC)))
           (GO WHILELABEL))
         (SETQ LHS* (PLUSDF LHS* (NEGDF REDU)))
         (COND (NEEDSQUASH (SETQ LHS* (SQUASHCONSTANTS LHS*)))))))
      (RETURN (CAR U)))) 
(PUT 'KILLSINGLES 'NUMBER-OF-ARGS 2) 
(PUT 'KILLSINGLES 'DEFINED-ON-LINE '298) 
(PUT 'KILLSINGLES 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'KILLSINGLES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE KILLSINGLES (XX CC)
    (COND ((ATOM XX) NIL) ((NOT (EQ (CDR XX) NIL)) NIL)
          (T
           (PROG (DD)
             (SETQ DD (ASSOC (CAAAR XX) CC))
             (COND (DD (RETURN DD)))
             (RETURN (KILLSINGLES (CDAR XX) CC)))))) 
(PUT 'RMVE 'NUMBER-OF-ARGS 2) 
(PUT 'RMVE 'DEFINED-ON-LINE '307) 
(PUT 'RMVE 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'RMVE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RMVE (L X)
    (COND ((EQUAL (CAAR L) X) (CDR L)) (T (CONS (CAR L) (RMVE (CDR L) X))))) 
(PUT 'SUBDF 'NUMBER-OF-ARGS 3) 
(PUT 'SUBDF 'DEFINED-ON-LINE '310) 
(PUT 'SUBDF 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'SUBDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBDF (A B C)
    (COND ((EQUAL A NIL) NIL)
          (T
           (PROG (X)
             (SETQ X (SUBS2Q (SUBF (CAR (CDAR A)) (LIST (CONS C B)))))
             (COND ((EQUAL X (CONS NIL 1)) (RETURN (SUBDF (CDR A) B C)))
                   (T
                    (RETURN
                     (PLUSDF
                      (LIST
                       (CONS (CAAR A)
                             (CONS (CAR X) (*MULTF (CDR X) (CDR (CDAR A))))))
                      (SUBDF (CDR A) B C))))))))) 
(PUT 'TESTORD 'NUMBER-OF-ARGS 2) 
(PUT 'TESTORD 'DEFINED-ON-LINE '322) 
(PUT 'TESTORD 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'TESTORD 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TESTORD (A B)
    (COND ((NULL A) T) ((LEQ (CAR A) (CAR B)) (TESTORD (CDR A) (CDR B)))
          (T NIL))) 
(PUT 'TANSFROM 'NUMBER-OF-ARGS 4) 
(PUT 'TANSFROM 'DEFINED-ON-LINE '328) 
(PUT 'TANSFROM 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'TANSFROM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE TANSFROM (RHS Z INDEXLIST N)
    (COND ((NULL Z) NIL)
          (T
           (PROG (ZZ R RR ANS)
             (SETQ R RHS)
             (SETQ ZZ (CAR Z))
             (SETQ ANS 0)
             (COND
              ((AND (NOT (ATOM ZZ)) (EQUAL (CAR ZZ) 'TAN))
               (PROG ()
                WHILELABEL
                 (COND ((NOT R) (RETURN NIL)))
                 (PROGN
                  (SETQ RR (CAAR R))
                  (PROG (I)
                    (SETQ I 1)
                   LAB
                    (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                    (SETQ RR (CDR RR))
                    (SETQ I (PLUS2 I 1))
                    (GO LAB))
                  (COND
                   ((FIXP (CAAR RR))
                    (SETQ ANS
                            (MAX ANS
                                 (TANEXTRACT (CAR INDEXLIST)
                                  (PREPSQ (CDAR R)))))))
                  (SETQ R (CDR R))
                  NIL)
                 (GO WHILELABEL))))
             (RETURN
              (CONS ANS (TANSFROM RHS (CDR Z) (CDR INDEXLIST) (PLUS N 1)))))))) 
(PUT 'TANEXTRACT 'NUMBER-OF-ARGS 2) 
(PUT 'TANEXTRACT 'DEFINED-ON-LINE '347) 
(PUT 'TANEXTRACT 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'TANEXTRACT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE TANEXTRACT (VAR EXP)
    (PROG (ANS C0 C1)
      (SETQ ANS (CDR (COEFF1 EXP VAR NIL)))
      (COND
       ((AND (EQUAL (LENGTH ANS) 2) (NOT (EQUAL (CAR ANS) 0)))
        (PROGN
         (SETQ C0 (CAR ANS))
         (SETQ C1 (CADR ANS))
         (COND ((EQCAR C0 '*SQ) (SETQ C0 (CADR C0))) (T (SETQ C0 (CONS C0 1))))
         (COND ((EQCAR C1 '*SQ) (SETQ C1 (CADR C1))) (T (SETQ C1 (CONS C1 1))))
         (SETQ ANS (MULTSQ C0 (INVSQ C1)))
         (COND ((ATOM ANS) (RETURN 0)))
         (COND
          ((AND (EQUAL (CDR ANS) 1) (FIXP (CAR ANS)))
           (RETURN (MINUS (CAR ANS)))))
         (RETURN 0))))
      (RETURN 0))) 
(PUT 'COEFDF 'NUMBER-OF-ARGS 2) 
(PUT 'COEFDF 'DEFINED-ON-LINE '364) 
(PUT 'COEFDF 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'COEFDF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE COEFDF (Y U)
    (COND ((EQUAL Y NIL) NIL) ((EQUAL (CAAR Y) U) (CDAR Y))
          (T (COEFDF (CDR Y) U)))) 
(PUT 'PURGECONST 'NUMBER-OF-ARGS 2) 
(PUT 'PURGECONST 'DEFINED-ON-LINE '370) 
(PUT 'PURGECONST 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'PURGECONST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PURGECONST (A B)
    (COND ((NULL A) NIL) ((EQUAL (CAR A) B) (PURGECONST (CDR A) B))
          (T (CONS (CAR A) (PURGECONST (CDR A) B))))) 
(PUT 'MAXORDER 'NUMBER-OF-ARGS 3) 
(PUT 'MAXORDER 'DEFINED-ON-LINE '376) 
(PUT 'MAXORDER 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'MAXORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAXORDER (MINPOWERS Z N)
    (COND ((NULL Z) NIL)
          ((EQCAR (CAR Z) 'SQRT)
           (CONS 1 (MAXORDER (CDR MINPOWERS) (CDR Z) (PLUS N 1))))
          ((OR (ATOM (CAR Z)) (NEQ (CAAR Z) 'TAN))
           (CONS (PLUS (MAXFROM LHS* N) 1)
                 (MAXORDER (CDR MINPOWERS) (CDR Z) (PLUS N 1))))
          (T
           (CONS (MAX (CAR MINPOWERS) (MAXFROM LHS* N))
                 (MAXORDER (CDR MINPOWERS) (CDR Z) (PLUS N 1)))))) 
(PUT 'MAXFROM 'NUMBER-OF-ARGS 2) 
(PUT 'MAXFROM 'DEFINED-ON-LINE '386) 
(PUT 'MAXFROM 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'MAXFROM 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE MAXFROM (L N) (MAXFROM1 L (PLUS N 1) 0)) 
(PUT 'MAXFROM1 'NUMBER-OF-ARGS 3) 
(PUT 'MAXFROM1 'DEFINED-ON-LINE '388) 
(PUT 'MAXFROM1 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'MAXFROM1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE MAXFROM1 (L N V)
    (COND ((NULL L) V)
          (T (PROGN (SETQ V (MAX (NTH (CAAR L) N) V)) (MAXFROM1 (CDR L) N V))))) 
(PUT 'ADDCTOMAP 'NUMBER-OF-ARGS 1) 
(PUT 'ADDCTOMAP 'DEFINED-ON-LINE '393) 
(PUT 'ADDCTOMAP 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'ADDCTOMAP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADDCTOMAP (CC)
    (PROG (NCVAL)
      (SETQ CCOUNT (PLUS CCOUNT 1))
      (SETQ NCVAL (MKVECT CCOUNT))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE CCOUNT 1) I)) (RETURN NIL)))
        (PUTV NCVAL I (GETV CVAL I))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PUTV NCVAL CCOUNT (CONS NIL 1))
      (SETQ CVAL NCVAL)
      (SETQ CMAP (CONS (CONS CC CCOUNT) CMAP))
      (COND (*TRINT (PROGN (PRIN2 "Constant map changed to ") (PRINT CMAP))))
      (SETQ CMATRIX
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J CMATRIX)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (ADDTOVECTOR J)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (ADDTOVECTOR J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))) 
(PUT 'ADDTOVECTOR 'NUMBER-OF-ARGS 1) 
(PUT 'ADDTOVECTOR 'DEFINED-ON-LINE '406) 
(PUT 'ADDTOVECTOR 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'ADDTOVECTOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADDTOVECTOR (V)
    (PROG (VV)
      (SETQ VV (MKVECT CCOUNT))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE (DIFFERENCE CCOUNT 1) I)) (RETURN NIL)))
        (PUTV VV I (GETV V I))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PUTV VV CCOUNT NIL)
      (RETURN VV))) 
(PUT 'CHECKCOEFFTS 'NUMBER-OF-ARGS 2) 
(PUT 'CHECKCOEFFTS 'DEFINED-ON-LINE '414) 
(PUT 'CHECKCOEFFTS 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'CHECKCOEFFTS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CHECKCOEFFTS (CL INDV)
    (COND ((NULL CL) T)
          (T
           (PROG (RES)
             (SETQ RES (EVALUATECOEFFTS (CAR (CAR CL)) INDV))
             (COND ((NOT (OR (NULL RES) (EQUAL RES 0))) (RETURN NIL))
                   (T (RETURN (CHECKCOEFFTS (CDR CL) INDV)))))))) 
(PUT 'NEXTU 'NUMBER-OF-ARGS 2) 
(PUT 'NEXTU 'DEFINED-ON-LINE '425) 
(PUT 'NEXTU 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'NEXTU 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE NEXTU (LTRHS POWLHS)
    (COND ((NULL LTRHS) NIL)
          (T
           (PROG (INDLIST UCOEFFT)
             (SETQ INDLIST (SUBTRACTINDS POWLHS (CAR LTRHS) NIL))
             (COND ((NULL INDLIST) (RETURN NIL)))
             (SETQ UCOEFFT (EVALUATECOEFFTS (CAR (CDR LTRHS)) INDLIST))
             (COND ((OR (NULL UCOEFFT) (EQUAL UCOEFFT 0)) (RETURN NIL)))
             (RETURN (CONS INDLIST (CONS UCOEFFT (CDR (CDR LTRHS))))))))) 
(PUT 'SUBTRACTINDS 'NUMBER-OF-ARGS 3) 
(PUT 'SUBTRACTINDS 'DEFINED-ON-LINE '440) 
(PUT 'SUBTRACTINDS 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'SUBTRACTINDS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBTRACTINDS (POWLHS L SOFAR)
    (COND ((NULL L) (REVERSIP SOFAR))
          ((LESSP (DIFFERENCE (CAR POWLHS) (CAAR L)) 0) NIL)
          (T
           (SUBTRACTINDS (CDR POWLHS) (CDR L)
            (CONS (DIFFERENCE (CAR POWLHS) (CAAR L)) SOFAR))))) 
(PUT 'GAUSSELIMN 'NUMBER-OF-ARGS 2) 
(PUT 'GAUSSELIMN 'DEFINED-ON-LINE '450) 
(PUT 'GAUSSELIMN 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'GAUSSELIMN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GAUSSELIMN (EQUATION TOKILL)
    (PROG (NEWROW PIVOT)
      (COND ((EQUAL CCOUNT 0) (GO NOWAY)))
      (SETQ NEWROW (MKVECT CCOUNT))
      (SPREADC EQUATION NEWROW 1)
      (SUBST4ELIMINATEDCS NEWROW (REVERSE ORDEROFELIM) (REVERSE CMATRIX))
      (SETQ NEWROW (MAKEPRIM NEWROW))
      (SETQ PIVOT (FINDPIVOT NEWROW))
      (COND ((NULL PIVOT) (GO NOPIVOTFOUND)))
      (SETQ ORDEROFELIM (CONS PIVOT ORDEROFELIM))
      (SETQ CMATRIX (CONS NEWROW CMATRIX))
      (RETURN T)
     NOPIVOTFOUND
      (COND
       ((NULL (GETV NEWROW 0))
        (PROGN
         (COND
          (*TRINT
           (PROGN
            (PRIN2 "This equation adds no new information")
            (TERPRI)
            "This equation adds no new information")))
         (RETURN NIL))))
     NOWAY
      (SETQ BADPART (CONS TOKILL BADPART))
      (COND
       (*TRINT
        (PROGN
         (PROGN
          (PRIN2 "Inconsistency in equations for constants,")
          (TERPRI)
          "Inconsistency in equations for constants,")
         (PROGN
          (PRIN2 "  so non integrable")
          (TERPRI)
          "  so non integrable"))))
      (RETURN NIL))) 
(PUT 'MAKEPRIM 'NUMBER-OF-ARGS 1) 
(PUT 'MAKEPRIM 'DEFINED-ON-LINE '477) 
(PUT 'MAKEPRIM 'DEFINED-IN-FILE 'INT/ISOLVE.RED) 
(PUT 'MAKEPRIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MAKEPRIM (ROW)
    (PROG (G)
      (SETQ G (GETV ROW 0))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE CCOUNT I)) (RETURN NIL)))
        (SETQ G (GCDF G (GETV ROW I)))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (COND
       ((NEQ G 1)
        (PROG (I)
          (SETQ I 0)
         LAB
          (COND ((MINUSP (DIFFERENCE CCOUNT I)) (RETURN NIL)))
          (PUTV ROW I (QUOTF-FAIL (GETV ROW I) G))
          (SETQ I (PLUS2 I 1))
          (GO LAB))))
      (PROG (I)
        (SETQ I 0)
       LAB
        (COND ((MINUSP (DIFFERENCE CCOUNT I)) (RETURN NIL)))
        (PROGN
         (SETQ G (GETV ROW I))
         (COND
          ((AND G (NOT (OR (ATOM G) (ATOM (CAR G)))))
           (PUTV ROW I (CAR (RESIMP (CONS (ROOTEXTRACTF G) 1)))))))
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (RETURN ROW))) 
(ENDMODULE) 