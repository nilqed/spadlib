(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RLJSON)) 
(REVISION 'RLJSON "$Id: rljson.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'RLJSON "(c) 2019-2020 T. Sturm") 
(PUT 'RL_SERVICES2JSON 'NUMBER-OF-ARGS 1) 
(PUT 'RL_SERVICES2JSON 'DEFINED-ON-LINE '34) 
(PUT 'RL_SERVICES2JSON 'DEFINED-IN-FILE 'REDLOG/RLSUPPORT/RLJSON.RED) 
(PUT 'RL_SERVICES2JSON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RL_SERVICES2JSON (FILE)
    (PROG (L W LL)
      (SETQ L
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (SORT RL_SERVICES* 'ORDP))
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (PROGN
                            (SETQ W (GET S 'RL_AMSERVICE))
                            (COND
                             (W
                              (LIST
                               (OR (GET W 'JSON)
                                   (CONS (CONS 'DOMAIN "generic")
                                         (CONS (CONS 'NAME (LTO_AT2STR W))
                                               (RL_HELPMKDOCAL W NIL)))))))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (PROGN
                            (SETQ W (GET S 'RL_AMSERVICE))
                            (COND
                             (W
                              (LIST
                               (OR (GET W 'JSON)
                                   (CONS (CONS 'DOMAIN "generic")
                                         (CONS (CONS 'NAME (LTO_AT2STR W))
                                               (RL_HELPMKDOCAL W NIL)))))))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (SETQ LL (LINELENGTH 4096))
      (COND (FILE (OUT (LIST FILE))))
      (RL_SERVICES2JSONPRINTLIST L 0)
      (COND (FILE (SHUT (LIST FILE))))
      (LINELENGTH LL))) 
(PUT 'RL_BUILTINS2JSON 'NUMBER-OF-ARGS 1) 
(PUT 'RL_BUILTINS2JSON 'DEFINED-ON-LINE '48) 
(PUT 'RL_BUILTINS2JSON 'DEFINED-IN-FILE 'REDLOG/RLSUPPORT/RLJSON.RED) 
(PUT 'RL_BUILTINS2JSON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RL_BUILTINS2JSON (FILE)
    (PROG (L W LL)
      (SETQ L
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (SORT RL_BUILTINS* 'ORDP))
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (LIST
                            (CONS (CONS 'DOMAIN "generic")
                                  (CONS (CONS 'NAME (LTO_AT2STR S))
                                        (RL_HELPMKDOCALBUILTIN S NIL)))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (LIST
                            (CONS (CONS 'DOMAIN "generic")
                                  (CONS (CONS 'NAME (LTO_AT2STR S))
                                        (RL_HELPMKDOCALBUILTIN S NIL)))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (SETQ LL (LINELENGTH 4096))
      (COND (FILE (OUT (LIST FILE))))
      (RL_SERVICES2JSONPRINTLIST L 0)
      (COND (FILE (SHUT (LIST FILE))))
      (LINELENGTH LL))) 
(PUT 'RL_TYPES2JSON 'NUMBER-OF-ARGS 1) 
(PUT 'RL_TYPES2JSON 'DEFINED-ON-LINE '59) 
(PUT 'RL_TYPES2JSON 'DEFINED-IN-FILE 'REDLOG/RLSUPPORT/RLJSON.RED) 
(PUT 'RL_TYPES2JSON 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RL_TYPES2JSON (FILE)
    (PROG (TYPES BL CL KWL L LL)
      (PROG (S)
        (SETQ S (RL_TYPESTRINGS RL_SERVICES*))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (PROG (G147 G148)
              (SETQ G147 (RL_HELPOVERVIEWTYPESDECOMPOSE S))
              (SETQ G148 G147)
              (SETQ BL (CAR G147))
              (SETQ G147 (CDR G147))
              (SETQ CL (CAR G147))
              (SETQ G147 (CDR G147))
              (SETQ KWL (CAR G147))
              (SETQ G147 (CDR G147))
              (RETURN G148))
            (SETQ TYPES (UNION (UNION TYPES BL) CL))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SETQ L
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (SORT TYPES 'ORDP))
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (LIST
                            (CONS (CONS 'NAME S)
                                  (RL_HELPMKDOCALTYPE
                                   (LTO_DOWNCASE (LTO_STRING2ID S)) NIL))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ S (CDR S))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (S)
                           (LIST
                            (CONS (CONS 'NAME S)
                                  (RL_HELPMKDOCALTYPE
                                   (LTO_DOWNCASE (LTO_STRING2ID S)) NIL))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (SETQ LL (LINELENGTH 4096))
      (COND (FILE (OUT (LIST FILE))))
      (RL_SERVICES2JSONPRINTLIST L 0)
      (COND (FILE (SHUT (LIST FILE))))
      (LINELENGTH LL))) 
(PUT 'RL_SERVICES2JSONPRINTLIST 'NUMBER-OF-ARGS 2) 
(PUT 'RL_SERVICES2JSONPRINTLIST 'DEFINED-ON-LINE '74) 
(PUT 'RL_SERVICES2JSONPRINTLIST 'DEFINED-IN-FILE 'REDLOG/RLSUPPORT/RLJSON.RED) 
(PUT 'RL_SERVICES2JSONPRINTLIST 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RL_SERVICES2JSONPRINTLIST (L INDENT)
    (PROG (FIRST)
      (SETQ FIRST T)
      (PROG (S)
        (SETQ S L)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (COND
             (FIRST
              (PROGN
               (IOTO_PRIN2T "[")
               (SETQ INDENT (PLUS INDENT 2))
               (SETQ FIRST NIL)))
             (T (IOTO_PRIN2T ",")))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE INDENT I)) (RETURN NIL)))
              (IOTO_PRIN2 " ")
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (COND ((STRINGP S) (PRIN1 S))
                  (T (RL_SERVICES2JSONPRINT S INDENT)))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (IOTO_CTERPRI)
      (SETQ INDENT (DIFFERENCE INDENT 2))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE INDENT I)) (RETURN NIL)))
        (IOTO_PRIN2 " ")
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (IOTO_PRIN2 "]"))) 
(PUT 'RL_SERVICES2JSONPRINT 'NUMBER-OF-ARGS 2) 
(PUT 'RL_SERVICES2JSONPRINT 'DEFINED-ON-LINE '97) 
(PUT 'RL_SERVICES2JSONPRINT 'DEFINED-IN-FILE 'REDLOG/RLSUPPORT/RLJSON.RED) 
(PUT 'RL_SERVICES2JSONPRINT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RL_SERVICES2JSONPRINT (X INDENT)
    (PROG (L LL C)
      (COND ((NOT X) (RETURN (IOTO_PRIN2 "{}"))))
      (COND
       ((OR (IDP X) (NUMBERP X))
        (RETURN (IOTO_PRIN2 (LIST "\"" (LTO_AT2STR X) "\"")))))
      (COND
       ((STRINGP X)
        (PROGN
         (SETQ L (CDR (REVERSIP (CDR (EXPLODE X)))))
         (SETQ LL (LIST '|"|))
         (PROG ()
          WHILELABEL
           (COND ((NOT L) (RETURN NIL)))
           (PROGN
            (SETQ C (PROG1 (CAR L) (SETQ L (CDR L))))
            (COND
             ((AND (EQ C '|"|) L (EQ (CAR L) '|"|))
              (PROGN
               (PROGN (SETQ LL (CONS C LL)) C)
               (PROG (W1)
                 (SETQ W1 (PROG1 (CAR L) (SETQ L (CDR L))))
                 (SETQ LL (CONS W1 LL))
                 (RETURN W1))
               (PROGN (SETQ LL (CONS '|\\| LL)) '|\\|)))
             (T (PROGN (SETQ LL (CONS C LL)) C)))
            NIL)
           (GO WHILELABEL))
         (SETQ LL (CONS '|"| LL))
         (RETURN (IOTO_PRIN2 (LIST "\"" (COMPRESS LL) "\""))))))
      (COND ((ALISTP X) (RETURN (RL_SERVICES2JSONPRINTRECORD X INDENT))))
      (COND ((LISTP X) (RETURN (RL_SERVICES2JSONPRINTLIST X INDENT))))
      (REDERR (LIST "rl_services2jsonPrint: bad argument" X)))) 
(PUT 'RL_SERVICES2JSONPRINTRECORD 'NUMBER-OF-ARGS 2) 
(PUT 'RL_SERVICES2JSONPRINTRECORD 'DEFINED-ON-LINE '125) 
(PUT 'RL_SERVICES2JSONPRINTRECORD 'DEFINED-IN-FILE 'REDLOG/RLSUPPORT/RLJSON.RED) 
(PUT 'RL_SERVICES2JSONPRINTRECORD 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE RL_SERVICES2JSONPRINTRECORD (AL INDENT)
    (PROG (FIRST)
      (SETQ FIRST T)
      (PROG (PR)
        (SETQ PR AL)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (PROGN
            (COND
             (FIRST
              (PROGN
               (IOTO_PRIN2T "{")
               (SETQ INDENT (PLUS INDENT 2))
               (SETQ FIRST NIL)))
             (T (IOTO_PRIN2T ",")))
            (PROG (I)
              (SETQ I 1)
             LAB
              (COND ((MINUSP (DIFFERENCE INDENT I)) (RETURN NIL)))
              (IOTO_PRIN2 " ")
              (SETQ I (PLUS2 I 1))
              (GO LAB))
            (RL_SERVICES2JSONPRINT (CAR PR) INDENT)
            (IOTO_PRIN2 ": ")
            (RL_SERVICES2JSONPRINT (CDR PR) INDENT)))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (IOTO_CTERPRI)
      (SETQ INDENT (DIFFERENCE INDENT 2))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE INDENT I)) (RETURN NIL)))
        (IOTO_PRIN2 " ")
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (IOTO_PRIN2 "}"))) 
(PUT 'RL_SERVICES2JSONARGS 'NUMBER-OF-ARGS 1) 
(PUT 'RL_SERVICES2JSONARGS 'DEFINED-ON-LINE '147) 
(PUT 'RL_SERVICES2JSONARGS 'DEFINED-IN-FILE 'REDLOG/RLSUPPORT/RLJSON.RED) 
(PUT 'RL_SERVICES2JSONARGS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE RL_SERVICES2JSONARGS (DOCAL)
    (PROG (NAMES DEFAULTS)
      (SETQ NAMES (CDR (ATSOC 'NAMES DOCAL)))
      (SETQ DEFAULTS (CDR (ATSOC 'DEFAULTS DOCAL)))
      (RETURN
       (PROG (NAME FORALL-RESULT FORALL-ENDPTR)
         (SETQ NAME NAMES)
        STARTOVER
         (COND ((NULL NAME) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 ((LAMBDA (NAME)
                    (COND ((NOT (ATSOC NAME DEFAULTS)) (LIST NAME))))
                  (CAR NAME)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
         (SETQ NAME (CDR NAME))
         (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
        LOOPLABEL
         (COND ((NULL NAME) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 ((LAMBDA (NAME)
                    (COND ((NOT (ATSOC NAME DEFAULTS)) (LIST NAME))))
                  (CAR NAME)))
         (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
         (SETQ NAME (CDR NAME))
         (GO LOOPLABEL))))) 
(ENDMODULE) 