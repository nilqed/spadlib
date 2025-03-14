(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CEDIT)) 
(CREATE-PACKAGE '(CEDIT) '(UTIL)) 
(FLUID '(*MODE RPRIFN* RTERFN*)) 
(GLOBAL
 '($EOL$ *BLANKNOTOK* *EAGAIN *FULL CRBUF* CRBUF1* CRBUFLIS* ESC* INPUTBUFLIS*
   STATCOUNTER CEDIT-LOADED*)) 
(TERPRI) 
(PROG (M)
  (SETQ M
          '("*** cedit activated ***"
            "Until you try to use cedit Reduce will not be saving your input for"
            "it, so your first use of it will fail and display this message, but"
            "from then on all that you type is preserved and can be used with cedit"
            "in the usual manner."))
 LAB
  (COND ((NULL M) (RETURN NIL)))
  ((LAMBDA (M) (PROGN (PRIN2 M) (TERPRI))) (CAR M))
  (SETQ M (CDR M))
  (GO LAB)) 
(SETQ CEDIT-LOADED* T) 
(PUT 'RPLACW 'NUMBER-OF-ARGS 2) 
(PUT 'RPLACW 'DEFINED-ON-LINE '60) 
(PUT 'RPLACW 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'RPLACW 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RPLACW (U V)
    (COND ((OR (ATOM U) (ATOM V)) (ERRACH (LIST 'RPLACW U V)))
          (T (RPLACD (RPLACA U (CAR V)) (CDR V))))) 
(PUT 'CEDIT 'NUMBER-OF-ARGS 1) 
(PUT 'CEDIT 'DEFINED-ON-LINE '64) 
(PUT 'CEDIT 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'CEDIT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CEDIT (N)
    (PROG (X OCHAN)
      (COND ((NULL (TERMINALP)) (REDERR "Edit must be from a terminal")))
      (SETQ OCHAN (WRS NIL))
      (COND ((EQ N 'FN) (SETQ X (REVERSIP CRBUF*)))
            ((NULL N)
             (COND
              ((NULL CRBUFLIS*)
               (PROGN
                (SETQ STATCOUNTER (DIFFERENCE STATCOUNTER 1))
                (REDERR "No previous entry")))
              (T (SETQ X (CDAR CRBUFLIS*)))))
            ((SETQ X (ASSOC (CAR N) CRBUFLIS*))
             (SETQ X (CEDIT0 (CDR X) (CAR N))))
            (T
             (PROGN
              (SETQ STATCOUNTER (DIFFERENCE STATCOUNTER 1))
              (REDERR (LIST "Entry" (CAR N) "not found")))))
      (SETQ CRBUF* NIL)
      (SETQ X
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J X)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) J) (CAR J)) NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (J) J) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (TERPRI)
      (EDITP X)
      (TERPRI)
      (SETQ X (CEDIT1 X))
      (WRS OCHAN)
      (COND ((EQ X 'FAILED) NIL) (T (SETQ CRBUF1* X))))) 
(PUT 'CEDIT0 'NUMBER-OF-ARGS 2) 
(PUT 'CEDIT0 'DEFINED-ON-LINE '88) 
(PUT 'CEDIT0 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'CEDIT0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE CEDIT0 (U N)
    (PROG (X)
      (COND
       ((OR (NOT (SETQ X (ASSOC N INPUTBUFLIS*))) (EQ (SETQ X (CADR X)) *MODE))
        (RETURN U))
       (T (RETURN (APPEND (EXPLODE X) (APPEND (CDR (EXPLODE BLANK)) U))))))) 
(PUT 'CEDIT1 'NUMBER-OF-ARGS 1) 
(PUT 'CEDIT1 'DEFINED-ON-LINE '96) 
(PUT 'CEDIT1 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'CEDIT1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CEDIT1 (U)
    (PROG (X Y Z)
      (SETQ Z (SETPCHAR '>))
      (COND
       ((NOT *EAGAIN) (PROGN (PRIN2T "For help, type ?") (SETQ *EAGAIN T))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND U (EQ (CAR U) $EOL$))) (RETURN NIL)))
        (SETQ U (CDR U))
        (GO WHILELABEL))
      (SETQ U (APPEND U (LIST BLANK)))
      (COND (*FULL (EDITP U)))
     TOP
      (SETQ X U)
     A
      (SETQ Y (READCH))
      (COND ((OR (EQ Y '|p|) (EQ Y 'P)) (EDITP X))
            ((OR (EQ Y '|i|) (EQ Y 'I)) (EDITI X))
            ((OR (EQ Y '|c|) (EQ Y 'C)) (EDITC X))
            ((OR (EQ Y '|d|) (EQ Y 'D)) (EDITD X))
            ((OR (EQ Y '|f|) (EQ Y 'F)) (SETQ X (EDITF X NIL)))
            ((OR (EQ Y '|e|) (EQ Y 'E))
             (PROGN (TERPRI) (EDITP1 U) (SETPCHAR Z) (RETURN U)))
            ((OR (EQ Y '|q|) (EQ Y 'Q)) (PROGN (SETPCHAR Z) (RETURN 'FAILED)))
            ((EQ Y '?) (EDITH)) ((OR (EQ Y '|b|) (EQ Y 'B)) (GO TOP))
            ((OR (EQ Y '|k|) (EQ Y 'K)) (EDITF X T))
            ((OR (EQ Y '|s|) (EQ Y 'S)) (SETQ X (EDITS X)))
            ((OR (AND (EQ Y BLANK) (NOT *BLANKNOTOK*)) (EQ Y '|x|) (EQ Y 'X))
             (SETQ X (EDITN X)))
            ((AND (EQ Y BLANK) *BLANKNOTOK*) (GO A)) ((EQ Y $EOL$) (GO A))
            (T (LPRIM* (LIST Y "Invalid editor character"))))
      (GO A))) 
(PUT 'EDITC 'NUMBER-OF-ARGS 1) 
(PUT 'EDITC 'DEFINED-ON-LINE '128) 
(PUT 'EDITC 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITC 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITC (X)
    (COND ((NULL (CDR X)) (LPRIM* "No more characters"))
          (T (RPLACA X (READCH))))) 
(PUT 'EDITD 'NUMBER-OF-ARGS 1) 
(PUT 'EDITD 'DEFINED-ON-LINE '132) 
(PUT 'EDITD 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITD (X)
    (COND ((NULL (CDR X)) (LPRIM* "No more characters"))
          (T (RPLACW X (CONS (CADR X) (CDDR X)))))) 
(PUT 'EDITF 'NUMBER-OF-ARGS 2) 
(PUT 'EDITF 'DEFINED-ON-LINE '136) 
(PUT 'EDITF 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDITF (X BOOL)
    (PROG (Y Z)
      (SETQ Y (CDR X))
      (SETQ Z (READCH))
      (COND ((NULL Y) (RETURN (PROGN (LPRIM* (LIST Z "Not found")) X))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (CDR Y) (NOT (EQ Z (CAR Y))))) (RETURN NIL)))
        (SETQ Y (CDR Y))
        (GO WHILELABEL))
      (RETURN
       (COND ((NULL (CDR Y)) (PROGN (LPRIM* (LIST Z "Not found")) X))
             (BOOL (RPLACW X (CONS (CAR Y) (CDR Y)))) (T Y))))) 
(PUT 'EDITH 'NUMBER-OF-ARGS 0) 
(PUT 'EDITH 'DEFINED-ON-LINE '147) 
(PUT 'EDITH 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITH 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE EDITH NIL
    (PROGN
     (PRIN2T "THE FOLLOWING COMMANDS ARE SUPPORTED:")
     (PRIN2T "   B              move pointer to beginning")
     (PRIN2T "   C<character>   replace next character by <character>")
     (PRIN2T "   D              delete next character")
     (PRIN2T "   E              end editing and reread text")
     (PRIN2T
      "   F<character>   move pointer to next occurrence of <character>")
     (PRIN2T "   I<string><escape>   insert <string> in front of pointer")
     (PRIN2T "   K<character>   delete all chars until <character>")
     (PRIN2T "   P              print string from current pointer")
     (PRIN2T "   Q              give up with error exit")
     (PRIN2T "   S<string><escape> search for first occurrence of <string>")
     (PRIN2T "                      positioning pointer just before it")
     (PRIN2T "   <space> or X   move pointer right one character")
     (TERPRI)
     (PRIN2T "ALL COMMAND SEQUENCES SHOULD BE FOLLOWED BY A CARRIAGE RETURN")
     (PRIN2T "    TO BECOME EFFECTIVE"))) 
(PUT 'EDITI 'NUMBER-OF-ARGS 1) 
(PUT 'EDITI 'DEFINED-ON-LINE '169) 
(PUT 'EDITI 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITI 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITI (X)
    (PROG (Y Z)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ (SETQ Y (READCH)) ESC*)) (RETURN NIL)))
        (SETQ Z (CONS Y Z))
        (GO WHILELABEL))
      (RPLACW X (NCONC (REVERSIP Z) (CONS (CAR X) (CDR X)))))) 
(PUT 'EDITN 'NUMBER-OF-ARGS 1) 
(PUT 'EDITN 'DEFINED-ON-LINE '175) 
(PUT 'EDITN 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITN (X) (COND ((NULL (CDR X)) (LPRIM* "NO MORE CHARACTERS")) (T (CDR X)))) 
(PUT 'EDITP 'NUMBER-OF-ARGS 1) 
(PUT 'EDITP 'DEFINED-ON-LINE '179) 
(PUT 'EDITP 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITP (U) (PROGN (EDITP1 U) (TERPRI))) 
(PUT 'EDITP1 'NUMBER-OF-ARGS 1) 
(PUT 'EDITP1 'DEFINED-ON-LINE '182) 
(PUT 'EDITP1 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITP1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITP1 (U)
    (PROG (X)
      (SETQ X U)
     LAB
      (COND ((NULL X) (RETURN NIL)))
      ((LAMBDA (X) (COND ((EQ X $EOL$) (TERPRI)) (T (PRIN2 X)))) (CAR X))
      (SETQ X (CDR X))
      (GO LAB))) 
(PUT 'EDITS 'NUMBER-OF-ARGS 1) 
(PUT 'EDITS 'DEFINED-ON-LINE '185) 
(PUT 'EDITS 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITS (U)
    (PROG (X Y Z)
      (SETQ X U)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ (SETQ Y (READCH)) ESC*)) (RETURN NIL)))
        (SETQ Z (CONS Y Z))
        (GO WHILELABEL))
      (SETQ Z (REVERSIP Z))
     A
      (COND ((NULL X) (RETURN (PROGN (LPRIM* "not found") U)))
            ((EDMATCH Z X) (RETURN X)))
      (SETQ X (CDR X))
      (GO A))) 
(PUT 'EDMATCH 'NUMBER-OF-ARGS 2) 
(PUT 'EDMATCH 'DEFINED-ON-LINE '196) 
(PUT 'EDMATCH 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDMATCH 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EDMATCH (U V)
    (COND ((NULL U) V) ((NULL V) NIL)
          ((EQUAL (CAR U) (CAR V)) (EDMATCH (CDR U) (CDR V))) (T NIL))) 
(PUT 'LPRIM* 'NUMBER-OF-ARGS 1) 
(PUT 'LPRIM* 'DEFINED-ON-LINE '204) 
(PUT 'LPRIM* 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'LPRIM* 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LPRIM* (U) (PROGN (LPRIM U) (TERPRI))) 
(REMPROP 'EDITDEF 'STAT) 
(PUT 'EDITDEF 'NUMBER-OF-ARGS 1) 
(PUT 'EDITDEF 'DEFINED-ON-LINE '210) 
(PUT 'EDITDEF 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITDEF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITDEF (U) (EDITDEF1 (CAR U))) 
(PUT 'EDITDEF1 'NUMBER-OF-ARGS 1) 
(PUT 'EDITDEF1 'DEFINED-ON-LINE '212) 
(PUT 'EDITDEF1 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'EDITDEF1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDITDEF1 (U)
    (PROG (TYPE X)
      (COND ((NULL (SETQ X (GETD U))) (RETURN (LPRIM (LIST U "not defined"))))
            ((OR (CODEP (CDR X)) (NOT (EQCAR (CDR X) 'LAMBDA)))
             (RETURN (LPRIM (LIST U "cannot be edited")))))
      (SETQ TYPE (CAR X))
      (SETQ X (CDR X))
      (COND ((EQ TYPE 'EXPR) (SETQ X (CONS 'DE (CONS U (CDR X)))))
            ((EQ TYPE 'FEXPR) (SETQ X (CONS 'DF (CONS U (CDR X)))))
            ((EQ TYPE 'MACRO) (SETQ X (CONS 'DM (CONS U (CDR X)))))
            (T (REDERR (LIST "strange function type" TYPE))))
      (SETQ RPRIFN* 'ADD2BUF)
      (SETQ RTERFN* 'ADDTER2BUF)
      (SETQ CRBUF* NIL)
      (SETQ X (ERRORSET* (LIST 'RPRINT (MKQUOTE X)) T))
      (SETQ RPRIFN* NIL)
      (SETQ RTERFN* NIL)
      (COND ((ERRORP X) (RETURN (SETQ CRBUF* NIL))))
      (SETQ CRBUF* (CEDIT 'FN))
      (RETURN NIL))) 
(PUT 'ADD2BUF 'NUMBER-OF-ARGS 1) 
(PUT 'ADD2BUF 'DEFINED-ON-LINE '234) 
(PUT 'ADD2BUF 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'ADD2BUF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ADD2BUF (U) (SETQ CRBUF* (CONS U CRBUF*))) 
(PUT 'ADDTER2BUF 'NUMBER-OF-ARGS 0) 
(PUT 'ADDTER2BUF 'DEFINED-ON-LINE '236) 
(PUT 'ADDTER2BUF 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'ADDTER2BUF 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE ADDTER2BUF NIL (SETQ CRBUF* (CONS $EOL$ CRBUF*))) 
(PUT 'EDITDEF 'STAT 'RLIS) 
(PUT 'DISPLAY 'STAT 'RLIS) 
(PUT 'DISPLAY 'NUMBER-OF-ARGS 1) 
(PUT 'DISPLAY 'DEFINED-ON-LINE '244) 
(PUT 'DISPLAY 'DEFINED-IN-FILE 'MISC/CEDIT.RED) 
(PUT 'DISPLAY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DISPLAY (U)
    (PROG (X W)
      (SETQ U (CAR U))
      (SETQ X CRBUFLIS*)
      (TERPRI)
      (COND ((NOT (NUMBERP U)) (SETQ U (LENGTH X))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND (GREATERP U 0) X)) (RETURN NIL)))
        (PROGN
         (SETQ W (CONS (CAR X) W))
         (SETQ X (CDR X))
         (SETQ U (DIFFERENCE U 1)))
        (GO WHILELABEL))
      (PROG (J)
        (SETQ J W)
       LAB
        (COND ((NULL J) (RETURN NIL)))
        ((LAMBDA (J)
           (PROGN (PRIN2 (CAR J)) (PRIN2 ": ") (EDITP (CDR J)) (TERPRI)))
         (CAR J))
        (SETQ J (CDR J))
        (GO LAB)))) 
(ENDMODULE) 