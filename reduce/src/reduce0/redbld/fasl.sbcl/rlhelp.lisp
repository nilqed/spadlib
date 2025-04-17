(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RLHELP)) 
(REVISION 'RLHELP "$Id: rlhelp.red 5986 2021-08-28 13:35:27Z thomas-sturm $") 
(COPYRIGHT 'RLHELP "(c) 2016-2017 T. Sturm") 
(GLOBAL '(RLHELP_LEFTMARGIN*)) 
(GLOBAL '(RLHELP_RIGHTMARGIN*)) 
(GLOBAL '(RLHELP_COLSEP*)) 
(SETQ RLHELP_LEFTMARGIN* 4) 
(SETQ RLHELP_RIGHTMARGIN* 1) 
(SETQ RLHELP_COLSEP* 2) 
(PUT '? 'STAT 'RL_HELPSTAT) 
(FLAG '(?) 'GO) 
(FLAG '(RL_HELP) 'NOFORM) 
(PUT 'RL_HELPSTAT 'NUMBER-OF-ARGS 0) 
(DE RL_HELPSTAT NIL
    (PROG (DEVP QUERY)
      (SCAN)
      (COND ((EQ CURSYM* '?) (PROGN (SETQ DEVP T) (SCAN))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (EQ CURSYM* '?)) (RETURN NIL)))
        (SCAN)
        (GO WHILELABEL))
      (COND ((FLAGP CURSYM* 'DELIM) (RETURN (LIST 'RL_HELP NIL DEVP))))
      (SETQ QUERY CURSYM*)
      (COND
       ((NOT (FLAGP (SCAN) 'DELIM))
        (REDERR (LIST "expecting delimiter but found" CURSYM*))))
      (RETURN (LIST 'RL_HELP (MKQUOTE QUERY) DEVP)))) 
(PUT 'RL_HELP 'NUMBER-OF-ARGS 2) 
(DE RL_HELP (X DEVP)
    (PROG ()
      (COND ((NULL X) (PROGN (RL_HELPOVERVIEW DEVP) (RETURN NIL))))
      (COND
       ((EQ X 'BUILTINS) (PROGN (RL_HELPOVERVIEWBUILTINS DEVP) (RETURN NIL))))
      (COND
       ((EQ X 'SERVICES) (PROGN (RL_HELPOVERVIEWSERVICES DEVP) (RETURN NIL))))
      (COND
       ((EQ X 'BLACKBOXES)
        (PROGN (RL_HELPOVERVIEWBLACKBOXES DEVP) (RETURN NIL))))
      (COND ((EQ X 'TYPES) (PROGN (RL_HELPOVERVIEWTYPES) (RETURN NIL))))
      (COND
       ((AND (EQCAR X 'QUOTIENT) (EQN (CADDR X) (RL_HELPTYPEARITY (CADR X))))
        (SETQ X (CADR X))))
      (COND ((RL_BUILTINP X) (PROGN (RL_HELPBUILTIN X DEVP) (RETURN NIL))))
      (COND ((RL_AMSERVICEP X) (PROGN (RL_HELPAMSERVICE X DEVP) (RETURN NIL))))
      (COND ((RL_SMSERVICEP X) (PROGN (RL_HELPSMSERVICE X DEVP) (RETURN NIL))))
      (COND ((RL_BLACKBOXP X) (PROGN (RL_HELPBLACKBOX X DEVP) (RETURN NIL))))
      (COND ((RL_TYPEP X) (PROGN (RL_HELPTYPE X DEVP) (RETURN NIL)))))) 
(PUT 'RL_HELPOVERVIEW 'NUMBER-OF-ARGS 1) 
(DE RL_HELPOVERVIEW (DEVP)
    (PROG (SL W ASL SSL BL CL KWL TYPES KEYWORDS)
      (SETQ DEVP NIL)
      (IOTO_TPRIN2T "REDLOG BUILTINS")
      (RL_HELPOVERVIEWCSL
       (PROG (S FORALL-RESULT FORALL-ENDPTR)
         (SETQ S RL_BUILTINS*)
         (COND ((NULL S) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (S) (LTO_AT2STR S)) (CAR S)) NIL)))
        LOOPLABEL
         (SETQ S (CDR S))
         (COND ((NULL S) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (S) (LTO_AT2STR S)) (CAR S)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))
      (TERPRI)
      (IOTO_TPRIN2T "REDLOG SERVICES")
      (PROG (S)
        (SETQ S RL_SERVICES*)
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (SETQ W (GET S 'RL_AMSERVICE))
            (COND
             (W
              (PROG (W1)
                (SETQ W1 (LTO_AT2STR W))
                (SETQ ASL (CONS W1 ASL))
                (RETURN W1)))
             (T
              (PROG (W1)
                (SETQ W1 (LTO_AT2STR S))
                (SETQ SSL (CONS W1 SSL))
                (RETURN W1))))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (RL_HELPOVERVIEWCSL ASL)
      (TERPRI)
      (COND
       ((AND DEVP SSL)
        (PROGN
         (IOTO_TPRIN2T "REDLOG SM-ONLY SERVICES")
         (RL_HELPOVERVIEWCSL SSL)
         (TERPRI))))
      (COND
       (DEVP
        (PROGN
         (IOTO_TPRIN2T "REDLOG BLACKBOXES")
         (RL_HELPOVERVIEWCSL
          (PROG (S FORALL-RESULT FORALL-ENDPTR)
            (SETQ S RL_BLACKBOXES*)
            (COND ((NULL S) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (S) (LTO_AT2STR S)) (CAR S)) NIL)))
           LOOPLABEL
            (SETQ S (CDR S))
            (COND ((NULL S) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (S) (LTO_AT2STR S)) (CAR S)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))
         NIL
         (TERPRI))))
      (PROG (S)
        (SETQ S (RL_TYPESTRINGS RL_SERVICES*))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (PROG (G141 G142)
              (SETQ G141 (RL_HELPOVERVIEWTYPESDECOMPOSE S))
              (SETQ G142 G141)
              (SETQ BL (CAR G141))
              (SETQ G141 (CDR G141))
              (SETQ CL (CAR G141))
              (SETQ G141 (CDR G141))
              (SETQ KWL (CAR G141))
              (SETQ G141 (CDR G141))
              (RETURN G142))
            (PROG (X)
              (SETQ X BL)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (SETQ TYPES (LTO_INSERT X TYPES))) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (PROG (X)
              (SETQ X CL)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (SETQ TYPES (LTO_INSERT (RL_HELPTYPECONCARITY X) TYPES)))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (PROG (X)
              (SETQ X KWL)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (SETQ KEYWORDS (LTO_INSERT X KEYWORDS))) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (IOTO_TPRIN2T "REDLOG TYPES")
      (RL_HELPOVERVIEWCSL TYPES)
      (TERPRI)
      (IOTO_TPRIN2T "REDLOG KEYWORDS")
      (RL_HELPOVERVIEWCSL KEYWORDS)
      (TERPRI)
      (IOTO_TPRIN2T "SEE ALSO")
      (RL_PRINTDESCRIPTIONLIST
       (COND
        (DEVP
         '(("??builtins" . "more information on builtins")
           ("??services" . "more information on services")
           ("??blackboxes" . "more information on blackboxes")
           ("??types" . "more information on types")
           ("??switches" . "more information on switches")
           ("??X" . "for a specific service, blackbox, type, or switch X")))
        (T
         '(("?builtins" . "more information on builtins")
           ("?services" . "more information on services")
           ("?types" . "more information on types")
           ("?X" . "for a specific service, type, or switch X"))))
       NIL))) 
(PUT 'RL_HELPOVERVIEWCSL 'NUMBER-OF-ARGS 1) 
(DE RL_HELPOVERVIEWCSL (L)
    (PROG (SL)
      (PROG (REST)
        (SETQ REST (SORT L 'LTO_STRINGGREATERP))
       LAB
        (COND ((NULL REST) (RETURN NIL)))
        (PROGN
         (PROG (W1) (SETQ W1 (CAR REST)) (SETQ SL (CONS W1 SL)) (RETURN W1))
         (COND ((CDR REST) (PROGN (SETQ SL (CONS ", " SL)) ", "))))
        (SETQ REST (CDR REST))
        (GO LAB))
      (RL_PRINTPARAGRAPH (LTO_SCONCAT SL)))) 
(PUT 'RL_HELPOVERVIEWBUILTINS 'NUMBER-OF-ARGS 1) 
(DE RL_HELPOVERVIEWBUILTINS (DEVP)
    (PROG (BL W L)
      (SETQ BL (SORT RL_BUILTINS* 'ORDP))
      (SETQ L
              (PROG (B FORALL-RESULT FORALL-ENDPTR)
                (SETQ B BL)
                (COND ((NULL B) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (B)
                                    (CONS (LTO_AT2STR B)
                                          (CDR
                                           (ATSOC 'DESCRIPTION
                                                  (CDR
                                                   (ATSOC 'DOC
                                                          (GET B
                                                               'RL_BUILTIN)))))))
                                  (CAR B))
                                 NIL)))
               LOOPLABEL
                (SETQ B (CDR B))
                (COND ((NULL B) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (B)
                            (CONS (LTO_AT2STR B)
                                  (CDR
                                   (ATSOC 'DESCRIPTION
                                          (CDR
                                           (ATSOC 'DOC
                                                  (GET B 'RL_BUILTIN)))))))
                          (CAR B))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (IOTO_TPRIN2T "REDLOG BUILTINS")
      (RL_PRINTDESCRIPTIONLIST L NIL)
      (TERPRI)
      (IOTO_TPRIN2T "SEE ALSO")
      (RL_PRINTDESCRIPTIONLIST '(("?X" . "for a specific builtin X"))
       (PROG (PR FORALL-RESULT FORALL-ENDPTR)
         (SETQ PR L)
         (COND ((NULL PR) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
        LOOPLABEL
         (SETQ PR (CDR PR))
         (COND ((NULL PR) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'RL_HELPOVERVIEWSERVICES 'NUMBER-OF-ARGS 1) 
(DE RL_HELPOVERVIEWSERVICES (DEVP)
    (PROG (SL W AL)
      (SETQ SL (SORT RL_SERVICES* 'ORDP))
      (SETQ AL
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S SL)
               STARTOVER
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (S)
                           (PROGN
                            (SETQ W (GET S 'RL_AMSERVICE))
                            (COND
                             (W
                              (LIST
                               (CONS (LTO_AT2STR W)
                                     (CDR
                                      (ATSOC 'DESCRIPTION
                                             (RL_HELPGETDOCAL W 'DOCAL)))))))))
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
                               (CONS (LTO_AT2STR W)
                                     (CDR
                                      (ATSOC 'DESCRIPTION
                                             (RL_HELPGETDOCAL W 'DOCAL)))))))))
                         (CAR S)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ S (CDR S))
                (GO LOOPLABEL)))
      (IOTO_TPRIN2T "REDLOG SERVICES")
      (RL_PRINTDESCRIPTIONLIST AL NIL)
      (TERPRI)
      (COND
       (DEVP
        (PROGN
         (SETQ AL
                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                   (SETQ S SL)
                  STARTOVER
                   (COND ((NULL S) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (S)
                              (COND
                               ((NOT (GET S 'RL_AMSERVICE))
                                (LIST
                                 (CONS (LTO_AT2STR S)
                                       (CDR
                                        (ATSOC 'DESCRIPTION
                                               (GET S 'DOCAL))))))))
                            (CAR S)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ S (CDR S))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL S) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (S)
                              (COND
                               ((NOT (GET S 'RL_AMSERVICE))
                                (LIST
                                 (CONS (LTO_AT2STR S)
                                       (CDR
                                        (ATSOC 'DESCRIPTION
                                               (GET S 'DOCAL))))))))
                            (CAR S)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ S (CDR S))
                   (GO LOOPLABEL)))
         (COND
          (AL
           (PROGN
            (IOTO_TPRIN2T "REDLOG SM-ONLY SERVICES")
            (RL_PRINTDESCRIPTIONLIST AL NIL)
            (TERPRI)))))))
      (IOTO_TPRIN2T "SEE ALSO")
      (RL_PRINTDESCRIPTIONLIST '(("?X" . "for a specific service X"))
       (PROG (PR FORALL-RESULT FORALL-ENDPTR)
         (SETQ PR AL)
         (COND ((NULL PR) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
        LOOPLABEL
         (SETQ PR (CDR PR))
         (COND ((NULL PR) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'RL_HELPOVERVIEWBLACKBOXES 'NUMBER-OF-ARGS 1) 
(DE RL_HELPOVERVIEWBLACKBOXES (DEVP)
    (PROG (AL)
      (COND ((NOT DEVP) (RETURN NIL)))
      (SETQ AL
              (PROG (S FORALL-RESULT FORALL-ENDPTR)
                (SETQ S (SORT RL_BLACKBOXES* 'ORDP))
                (COND ((NULL S) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S)
                                    (CONS (ID2STRING S)
                                          (CDR
                                           (ATSOC 'DESCRIPTION
                                                  (GET S 'DOCAL)))))
                                  (CAR S))
                                 NIL)))
               LOOPLABEL
                (SETQ S (CDR S))
                (COND ((NULL S) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (S)
                            (CONS (ID2STRING S)
                                  (CDR (ATSOC 'DESCRIPTION (GET S 'DOCAL)))))
                          (CAR S))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (IOTO_TPRIN2T "REDLOG BLACKBOXES")
      (RL_PRINTDESCRIPTIONLIST AL NIL)
      (TERPRI)
      (IOTO_TPRIN2T "SEE ALSO")
      (RL_PRINTDESCRIPTIONLIST '(("?X" . "for a specific blackbox X"))
       (PROG (PR FORALL-RESULT FORALL-ENDPTR)
         (SETQ PR AL)
         (COND ((NULL PR) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
        LOOPLABEL
         (SETQ PR (CDR PR))
         (COND ((NULL PR) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'RL_HELPOVERVIEWTYPES 'NUMBER-OF-ARGS 0) 
(DE RL_HELPOVERVIEWTYPES NIL
    (PROG (BL CL BAL CAL SAL XDTL KWL1 KWL)
      (PROG (S)
        (SETQ S (RL_TYPESTRINGS RL_SERVICES*))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (PROGN
            (PROG (G143 G144)
              (SETQ G143 (RL_HELPOVERVIEWTYPESDECOMPOSE S))
              (SETQ G144 G143)
              (SETQ BL (CAR G143))
              (SETQ G143 (CDR G143))
              (SETQ CL (CAR G143))
              (SETQ G143 (CDR G143))
              (SETQ KWL1 (CAR G143))
              (SETQ G143 (CDR G143))
              (RETURN G144))
            (PROG (X)
              (SETQ X BL)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (SETQ BAL
                         (LTO_INSERT
                          (CONS X
                                (OR
                                 (LTO_CATSOC 'DESCRIPTION
                                             (RL_TYPEDOC
                                              (LTO_DOWNCASE
                                               (LTO_STRING2ID X))))
                                 ""))
                          BAL)))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (PROG (X)
              (SETQ X CL)
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X)
                 (SETQ CAL
                         (LTO_INSERT
                          (CONS (RL_HELPTYPECONCARITY X)
                                (OR
                                 (LTO_CATSOC 'DESCRIPTION
                                             (RL_TYPEDOC
                                              (LTO_DOWNCASE
                                               (LTO_STRING2ID X))))
                                 ""))
                          CAL)))
               (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (SETQ KWL (UNION KWL KWL1))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (SETQ BAL
              (SORT BAL
                    (FUNCTION (LAMBDA (X Y) (LTO_STRINGLEQ (CAR X) (CAR Y))))))
      (SETQ CAL
              (SORT CAL
                    (FUNCTION (LAMBDA (X Y) (LTO_STRINGLEQ (CAR X) (CAR Y))))))
      (SETQ SAL '(("?X" . "for a specific type X")))
      (SETQ XDTL
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR (APPEND SAL (APPEND BAL CAL)))
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL)))
               LOOPLABEL
                (SETQ PR (CDR PR))
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (PR) (CAR PR)) (CAR PR)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (IOTO_TPRIN2T "REDLOG BASIC TYPES")
      (RL_PRINTDESCRIPTIONLIST BAL XDTL)
      (TERPRI)
      (IOTO_TPRIN2T "REDLOG COMPOUND TYPES")
      (RL_PRINTDESCRIPTIONLIST CAL XDTL)
      (TERPRI)
      (IOTO_TPRIN2T "REDLOG KEYWORDS")
      (RL_HELPOVERVIEWCSL KWL)
      (TERPRI)
      (IOTO_TPRIN2T "SEE ALSO")
      (RL_PRINTDESCRIPTIONLIST SAL XDTL))) 
(PUT 'RL_HELPOVERVIEWTYPESDECOMPOSE 'NUMBER-OF-ARGS 1) 
(DE RL_HELPOVERVIEWTYPESDECOMPOSE (S)
    ((LAMBDA (*LOWER *RAISE)
       (RL_HELPOVERVIEWTYPESDECOMPOSE1 (IOTO_SXREAD S) NIL NIL NIL))
     NIL NIL)) 
(PUT 'RL_HELPOVERVIEWTYPESDECOMPOSE1 'NUMBER-OF-ARGS 4) 
(DE RL_HELPOVERVIEWTYPESDECOMPOSE1 (X BL CL KWL)
    (PROG (CARXS)
      (COND ((IDP X) (RETURN (LIST (CONS (ID2STRING X) BL) CL KWL))))
      (SETQ CARXS (ID2STRING (CAR X)))
      (PROGN (SETQ CL (CONS CARXS CL)) CARXS)
      (COND
       ((EQUAL CARXS "Enum")
        (PROGN
         (PROG (KW)
           (SETQ KW (CDR X))
          LAB
           (COND ((NULL KW) (RETURN NIL)))
           ((LAMBDA (KW)
              (PROG (W1)
                (SETQ W1 (ID2STRING KW))
                (SETQ KWL (CONS W1 KWL))
                (RETURN W1)))
            (CAR KW))
           (SETQ KW (CDR KW))
           (GO LAB))
         (RETURN (LIST BL CL KWL)))))
      (PROG (Y)
        (SETQ Y (CDR X))
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROG (G145 G146)
             (SETQ G145 (RL_HELPOVERVIEWTYPESDECOMPOSE1 Y BL CL KWL))
             (SETQ G146 G145)
             (SETQ BL (CAR G145))
             (SETQ G145 (CDR G145))
             (SETQ CL (CAR G145))
             (SETQ G145 (CDR G145))
             (SETQ KWL (CAR G145))
             (SETQ G145 (CDR G145))
             (RETURN G146)))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN (LIST BL CL KWL)))) 
(PUT 'RL_HELPTYPECONCARITY 'NUMBER-OF-ARGS 1) 
(DE RL_HELPTYPECONCARITY (S)
    (LTO_SCONCAT
     (LIST S "/"
           (IOTO_SMAPRIN
            (RL_TYPEARITY (INTERN (LTO_DOWNCASE (COMPRESS (EXPLODE2 S))))))))) 
(PUT 'RL_TYPESTRINGS 'NUMBER-OF-ARGS 1) 
(DE RL_TYPESTRINGS (SERVICES)
    (PROG (W)
      (RETURN
       (LTO_LIST2SET
        (PROG (S FORALL-RESULT FORALL-ENDPTR)
          (SETQ S SERVICES)
         STARTOVER
          (COND ((NULL S) (RETURN NIL)))
          (SETQ FORALL-RESULT
                  ((LAMBDA (S)
                     (PROGN
                      (SETQ W (GET S 'RL_AMSERVICE))
                      (COND
                       (W (COPY (CONS (GET W 'OUTTYPE) (GET W 'INTYPES)))))))
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
                       (W (COPY (CONS (GET W 'OUTTYPE) (GET W 'INTYPES)))))))
                   (CAR S)))
          (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
          (SETQ S (CDR S))
          (GO LOOPLABEL)))))) 
(PUT 'RL_TYPEIDENTIFIERS 'NUMBER-OF-ARGS 1) 
(DE RL_TYPEIDENTIFIERS (SERVICES)
    (LTO_LIST2SET
     (PROG (S FORALL-RESULT FORALL-ENDPTR)
       (SETQ S SERVICES)
      STARTOVER
       (COND ((NULL S) (RETURN NIL)))
       (SETQ FORALL-RESULT
               ((LAMBDA (S)
                  (PROG (TP FORALL-RESULT FORALL-ENDPTR)
                    (SETQ TP (CONS (GET S 'OUTTYPE) (GET S 'INTYPES)))
                    (COND ((NULL TP) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (TP) (LTO_STRING2ID TP))
                                      (CAR TP))
                                     NIL)))
                   LOOPLABEL
                    (SETQ TP (CDR TP))
                    (COND ((NULL TP) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (TP) (LTO_STRING2ID TP)) (CAR TP))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
                (CAR S)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
       (SETQ S (CDR S))
       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
      LOOPLABEL
       (COND ((NULL S) (RETURN FORALL-RESULT)))
       (RPLACD FORALL-ENDPTR
               ((LAMBDA (S)
                  (PROG (TP FORALL-RESULT FORALL-ENDPTR)
                    (SETQ TP (CONS (GET S 'OUTTYPE) (GET S 'INTYPES)))
                    (COND ((NULL TP) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (TP) (LTO_STRING2ID TP))
                                      (CAR TP))
                                     NIL)))
                   LOOPLABEL
                    (SETQ TP (CDR TP))
                    (COND ((NULL TP) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (TP) (LTO_STRING2ID TP)) (CAR TP))
                                  NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL)))
                (CAR S)))
       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
       (SETQ S (CDR S))
       (GO LOOPLABEL)))) 
(PUT 'RL_HELPAMSERVICE 'NUMBER-OF-ARGS 2) 
(DE RL_HELPAMSERVICE (X DEVP)
    (PROG (DOCAL)
      (SETQ DOCAL (OR (GET X 'DOCAL) (RL_HELPMKDOCAL X DEVP)))
      (RL_HELPPRINTDOCAL DOCAL))) 
(PUT 'RL_HELPBUILTIN 'NUMBER-OF-ARGS 2) 
(DE RL_HELPBUILTIN (X DEVP)
    (PROG (DOCAL)
      (SETQ DOCAL (RL_HELPMKDOCALBUILTIN X DEVP))
      (RL_HELPPRINTDOCAL DOCAL))) 
(PUT 'RL_HELPSMSERVICE 'NUMBER-OF-ARGS 2) 
(DE RL_HELPSMSERVICE (X DEVP) NIL) 
(PUT 'RL_HELPBLACKBOX 'NUMBER-OF-ARGS 2) 
(DE RL_HELPBLACKBOX (X DEVP)
    (PROG (DOCAL L)
      (SETQ DOCAL (GET X 'DOCAL))
      (COND (DOCAL (RL_HELPPRINTDOCAL DOCAL)))
      (PROG (Y)
        (SETQ Y RL_SERVICES*)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (PROG (Z)
             (SETQ Z (RL_KNOWNIMPLEMENTATIONS Y))
            LAB
             (COND ((NULL Z) (RETURN NIL)))
             ((LAMBDA (Z)
                (COND
                 ((MEMQ X (RL_REGISTEREDBLACKBOXES Z))
                  (SETQ L (LTO_INSERTQ (LTO_AT2STR Z) L)))))
              (CAR Z))
             (SETQ Z (CDR Z))
             (GO LAB)))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (COND ((NULL L) (RETURN NIL)))
      (TERPRI)
      (IOTO_TPRIN2T "REGISTERED FOR (DYNAMICALLY)")
      (RL_HELPOVERVIEWCSL L))) 
(PUT 'RL_HELPGETDOCAL 'NUMBER-OF-ARGS 2) 
(DE RL_HELPGETDOCAL (X DEVP) (OR (GET X 'DOCAL) (RL_HELPMKDOCAL X DEVP))) 
(PUT 'RL_HELPMKDOCAL 'NUMBER-OF-ARGS 2) 
(DE RL_HELPMKDOCAL (X DEVP)
    (PROG (NAMES TYPES DEFAULTS DOCS DOCAL)
      (SETQ NAMES (GET X 'NAMES))
      (SETQ TYPES (GET X 'INTYPES))
      (SETQ DEFAULTS (GET X 'DEFAULTS))
      (SETQ DOCS (GET X 'DOCS))
      (SETQ DOCAL
              (LIST (CONS 'SYNOPSIS (RL_DOCSYNOPSIS X NAMES TYPES DEFAULTS))
                    (CONS 'DESCRIPTION (GET X 'DESCRIPTION))
                    (CONS 'RETURNS (GET X 'OUTTYPE))
                    (CONS 'ARGUMENTS (RL_DOCARGUMENTS NAMES TYPES DOCS))
                    (CONS 'SWITCHES (RL_DOCSWITCHES NAMES TYPES DOCS))
                    (CONS 'SEEALSO
                          (PROG (S FORALL-RESULT FORALL-ENDPTR)
                            (SETQ S (GET X 'SEEALSO))
                            (COND ((NULL S) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (S)
                                                (CONS S (GET S 'DESCRIPTION)))
                                              (CAR S))
                                             NIL)))
                           LOOPLABEL
                            (SETQ S (CDR S))
                            (COND ((NULL S) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS
                                     ((LAMBDA (S)
                                        (CONS S (GET S 'DESCRIPTION)))
                                      (CAR S))
                                     NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
      (RETURN DOCAL))) 
(PUT 'RL_HELPMKDOCALBUILTIN 'NUMBER-OF-ARGS 2) 
(DE RL_HELPMKDOCALBUILTIN (X DEVP)
    (PROG (RAWDAL W DAL)
      (SETQ RAWDAL (CDR (ATSOC 'DOC (GET X (GET X 'RL_SUPPORT)))))
      (SETQ W (CDR (ATSOC 'SYNOPSIS RAWDAL)))
      (SETQ W (SORT W (FUNCTION (LAMBDA (U V) (LESSP (CAR U) (CAR V))))))
      (PROG (W1)
        (SETQ W1
                (CONS 'SYNOPSIS
                      (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                        (SETQ PR W)
                        (COND ((NULL PR) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (PR) (CDR PR)) (CAR PR))
                                              NIL)))
                       LOOPLABEL
                        (SETQ PR (CDR PR))
                        (COND ((NULL PR) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (PR) (CDR PR)) (CAR PR)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
        (SETQ DAL (CONS W1 DAL))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (ATSOC 'DESCRIPTION RAWDAL))
        (SETQ DAL (CONS W1 DAL))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (ATSOC 'RETURNS RAWDAL))
        (SETQ DAL (CONS W1 DAL))
        (RETURN W1))
      (SETQ W (CDR (ATSOC 'ARGUMENTS RAWDAL)))
      (SETQ W (SORT W (FUNCTION (LAMBDA (U V) (LESSP (CAR U) (CAR V))))))
      (PROG (W1)
        (SETQ W1
                (CONS 'ARGUMENTS
                      (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                        (SETQ PR W)
                        (COND ((NULL PR) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (PR) (CDR PR)) (CAR PR))
                                              NIL)))
                       LOOPLABEL
                        (SETQ PR (CDR PR))
                        (COND ((NULL PR) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (PR) (CDR PR)) (CAR PR)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
        (SETQ DAL (CONS W1 DAL))
        (RETURN W1))
      (SETQ W (CDR (ATSOC 'SEEALSO RAWDAL)))
      (SETQ W (SORT W 'ORDP))
      (PROG (W1)
        (SETQ W1
                (CONS 'SEEALSO
                      (PROG (S FORALL-RESULT FORALL-ENDPTR)
                        (SETQ S W)
                        (COND ((NULL S) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS
                                         ((LAMBDA (S)
                                            (CONS S (RL_GETDESCRIPTION S)))
                                          (CAR S))
                                         NIL)))
                       LOOPLABEL
                        (SETQ S (CDR S))
                        (COND ((NULL S) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (S) (CONS S (RL_GETDESCRIPTION S)))
                                  (CAR S))
                                 NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
        (SETQ DAL (CONS W1 DAL))
        (RETURN W1))
      (RETURN (REVERSIP DAL)))) 
(PUT 'RL_HELPTYPE 'NUMBER-OF-ARGS 2) 
(DE RL_HELPTYPE (X DEVP)
    (PROG (DOCAL)
      (SETQ DOCAL (RL_HELPMKDOCALTYPE X DEVP))
      (RL_HELPPRINTDOCAL DOCAL))) 
(PUT 'RL_HELPMKDOCALTYPE 'NUMBER-OF-ARGS 2) 
(DE RL_HELPMKDOCALTYPE (X DEVP)
    (PROG (RAWDAL W DAL)
      (SETQ RAWDAL (CDR (ATSOC 'DOC (GET X (GET X 'RL_SUPPORT)))))
      (PROG (K)
        (SETQ K '(DESCRIPTION SYNTAX EXAMPLE))
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (PROGN
            (SETQ W (ATSOC K RAWDAL))
            (COND (W (PROGN (SETQ DAL (CONS W DAL)) W)))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (RETURN (REVERSIP DAL)))) 
(PUT 'RL_GETDESCRIPTION 'NUMBER-OF-ARGS 1) 
(DE RL_GETDESCRIPTION (X)
    (COND
     ((RL_BUILTINP X)
      (CDR
       (ATSOC 'DESCRIPTION (CDR (ATSOC 'DOC (GET X (GET X 'RL_SUPPORT)))))))
     ((RL_AMSERVICEP X) (GET X 'DESCRIPTION)))) 
(PUT 'RL_HELPPRINTDOCAL 'NUMBER-OF-ARGS 1) 
(DE RL_HELPPRINTDOCAL (DOCAL)
    (PROG (PR SL BRK XDTL L)
      (SETQ XDTL
              (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                (SETQ PR DOCAL)
               STARTOVER
                (COND ((NULL PR) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (PR)
                           (COND
                            ((ALISTP (CDR PR))
                             (PROG (PPR FORALL-RESULT FORALL-ENDPTR)
                               (SETQ PPR (CDR PR))
                               (COND ((NULL PPR) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (PPR) (CAR PPR))
                                                 (CAR PPR))
                                                NIL)))
                              LOOPLABEL
                               (SETQ PPR (CDR PPR))
                               (COND ((NULL PPR) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (PPR) (CAR PPR)) (CAR PPR))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))))
                         (CAR PR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ PR (CDR PR))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL PR) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (PR)
                           (COND
                            ((ALISTP (CDR PR))
                             (PROG (PPR FORALL-RESULT FORALL-ENDPTR)
                               (SETQ PPR (CDR PR))
                               (COND ((NULL PPR) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (PPR) (CAR PPR))
                                                 (CAR PPR))
                                                NIL)))
                              LOOPLABEL
                               (SETQ PPR (CDR PPR))
                               (COND ((NULL PPR) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (PPR) (CAR PPR)) (CAR PPR))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))))
                         (CAR PR)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ PR (CDR PR))
                (GO LOOPLABEL)))
      (PROG (REST)
        (SETQ REST DOCAL)
       LAB
        (COND ((NULL REST) (RETURN NIL)))
        (PROGN
         (SETQ PR (CAR REST))
         (COND
          ((CDR PR)
           (PROGN
            (COND (BRK (TERPRI)) (T (SETQ BRK T)))
            (COND
             ((EQUAL (CAR PR) 'SYNOPSIS)
              (PROGN
               (IOTO_TPRIN2T (LTO_UPCASE (ID2STRING (CAR PR))))
               (SETQ L (COND ((LISTP (CDR PR)) (CDR PR)) (T (LIST (CDR PR)))))
               (PROG (X)
                 (SETQ X L)
                LAB
                 (COND ((NULL X) (RETURN NIL)))
                 ((LAMBDA (X)
                    (PROGN
                     (PROG (I)
                       (SETQ I 1)
                      LAB
                       (COND
                        ((MINUSP (DIFFERENCE RLHELP_LEFTMARGIN* I))
                         (RETURN NIL)))
                       (PRIN2 " ")
                       (SETQ I (PLUS2 I 1))
                       (GO LAB))
                     (PRIN2T X)))
                  (CAR X))
                 (SETQ X (CDR X))
                 (GO LAB))))
             ((STRINGP (CDR PR))
              (PROGN
               (IOTO_TPRIN2T (LTO_UPCASE (ID2STRING (CAR PR))))
               (RL_PRINTPARAGRAPH (CDR PR))))
             (T
              (PROGN
               (IOTO_TPRIN2T
                (COND ((EQUAL (CAR PR) 'SEEALSO) "SEE ALSO")
                      (T (IOTO_TPRIN2T (LTO_UPCASE (ID2STRING (CAR PR)))))))
               (RL_PRINTDESCRIPTIONLIST (CDR PR) XDTL))))))))
        (SETQ REST (CDR REST))
        (GO LAB)))) 
(PUT 'RL_PRINTPARAGRAPH 'NUMBER-OF-ARGS 1) 
(DE RL_PRINTPARAGRAPH (S)
    (PROGN
     (PROG (L)
       (SETQ L (LTO_STRINGPARAGRAPHWRAPPER S))
      LAB
       (COND ((NULL L) (RETURN NIL)))
       ((LAMBDA (L) (PRIN2T L)) (CAR L))
       (SETQ L (CDR L))
       (GO LAB))
     NIL)) 
(PUT 'LTO_STRINGPARAGRAPHWRAPPER 'NUMBER-OF-ARGS 1) 
(DE LTO_STRINGPARAGRAPHWRAPPER (S)
    (LTO_STRINGPARAGRAPH S RLHELP_LEFTMARGIN*
                         (DIFFERENCE (LINELENGTH NIL) RLHELP_RIGHTMARGIN*))) 
(PUT 'RL_PRINTDESCRIPTIONLIST 'NUMBER-OF-ARGS 2) 
(DE RL_PRINTDESCRIPTIONLIST (AL XDTL)
    (PROGN
     (PROG (L)
       (SETQ L (LTO_STRINGDESCRIPTIONLISTWRAPPER AL XDTL))
      LAB
       (COND ((NULL L) (RETURN NIL)))
       ((LAMBDA (L) (PRIN2T L)) (CAR L))
       (SETQ L (CDR L))
       (GO LAB))
     NIL)) 
(PUT 'LTO_STRINGDESCRIPTIONLISTWRAPPER 'NUMBER-OF-ARGS 2) 
(DE LTO_STRINGDESCRIPTIONLISTWRAPPER (AL XDTL)
    (LTO_STRINGDESCRIPTIONLIST AL RLHELP_LEFTMARGIN* RLHELP_COLSEP*
                               (DIFFERENCE (LINELENGTH NIL)
                                           RLHELP_RIGHTMARGIN*)
                               XDTL)) 
(ENDMODULE) 