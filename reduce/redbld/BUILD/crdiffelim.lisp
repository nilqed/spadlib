(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'INTERFACE_DIFFELIM)) 
(FLUID '(*ALLFAC *NAT DEPL* *MSG *INT SEMIC* *ECHO *DIFFELIMVERBOSITY* EQNAME_)) 
(PUT 'CALL_DIFFELIM 'NUMBER-OF-ARGS 4) 
(PUT 'CALL_DIFFELIM 'DEFINED-ON-LINE '73) 
(PUT 'CALL_DIFFELIM 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'CALL_DIFFELIM 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE CALL_DIFFELIM (U V N L)
    (PROG (RES TMPF TMPFF ICHAN OLDICHAN SEMIC *MSG *INT XTRNLCALL SL A SAVE
           OFL*BAK)
      (COND
       ((NULL XTRNLCALL)
        (PROGN
         (COND ((NULL CRACK_LOAD_COMMAND) (CRACK_LOAD_CMD)))
         (SETQ SL
                 (COMPRESS
                  (REVERSE
                   (CONS '|"|
                         (CDDDDR
                          (CDDDR (REVERSE (EXPLODE CRACK_LOAD_COMMAND))))))))
         (COND
          (SL
           (SETQ XTRNLCALL
                   (BLDMSG_INTERNAL "%w/DiffElim-1.1/DiffElim" (LIST SL)))))
         NIL)))
      (COND ((NULL XTRNLCALL) (REDERR "DiffElim can not be found.")))
      (COND
       ((NULL (FILEP XTRNLCALL))
        (REDERR
         (LIST "No file " XTRNLCALL
               " was found. Probably DiffElim is not installed."
               " This should have been distributed with CRACK."))))
      (SYSTEM
       (BLDMSG_INTERNAL "%w%w%w"
                        (LIST XTRNLCALL " --help"
                              (COND ((GETENV "windir") " > NUL")
                                    (T " > /dev/null")))))
      (SETQ TMPF
              (COND ((GETENV "windir") "DiffElimIn.tmp")
                    (T
                     (BLDMSG_INTERNAL "%w%w%w"
                                      (LIST "/tmp/DiffElimIn_"
                                            (LEVEL_STRING SESSION_) "tmp")))))
      (SETQ A (OPEN TMPF 'OUTPUT))
      (SETQ OFL*BAK OFL*)
      (SETQ OFL* TMPF)
      (SETQ SAVE (WRS A))
      (SETQ SL (GEN_DIFFELIM_INPUT U N V))
      (WRS SAVE)
      (SETQ OFL* OFL*BAK)
      (CLOSE A)
      (COND (SL (UNMKALLFUNSTRS SL)))
      (SETQ XTRNLCALL (BLDMSG_INTERNAL "%w%w%w" (LIST XTRNLCALL " -f " TMPF)))
      (COND
       (L
        (SETQ XTRNLCALL
                (BLDMSG_INTERNAL "%w%w%w" (LIST XTRNLCALL " -haltiter " L)))))
      (COND
       (*DIFFELIMVERBOSITY*
        (SETQ XTRNLCALL
                (BLDMSG_INTERNAL "%w%w%w%w%w%w"
                                 (LIST XTRNLCALL " -displaylvl "
                                       *DIFFELIMVERBOSITY* " >> " TMPF
                                       ".log"))))
       (T
        (SETQ XTRNLCALL
                (BLDMSG_INTERNAL "%w%w"
                                 (LIST XTRNLCALL
                                       (COND ((GETENV "windir") " > NUL")
                                             (T " > /dev/null")))))))
      (COND
       ((GETENV "windir")
        (SYSTEM (BLDMSG_INTERNAL "%w%w" (LIST "dos2unix " TMPF)))))
      (SYSTEM XTRNLCALL)
      (SETQ TMPFF (BLDMSG_INTERNAL "%w%w" (LIST TMPF ".final")))
      (SYSTEM (BLDMSG_INTERNAL "%w%w" (LIST "echo end$ >> " TMPFF)))
      (SETQ ICHAN (OPEN (MKFIL* TMPFF) 'INPUT))
      (SETQ OLDICHAN (RDS ICHAN))
      (SETQ SEMIC SEMIC*)
      (COND (SL (UNARGFUNS SL)))
      (SETQ RES (READ_DIFFELIM_OUTPUT))
      (RDS OLDICHAN)
      (CLOSE ICHAN)
      (SETQ SEMIC* SEMIC)
      (COND (SL (CLEAR_UNARGFUNS SL)))
      (RETURN RES))) 
(PUT 'DIFFELIMVERBOSITY 'NUMBER-OF-ARGS 1) 
(PUT 'DIFFELIMVERBOSITY 'DEFINED-ON-LINE '164) 
(PUT 'DIFFELIMVERBOSITY 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'DIFFELIMVERBOSITY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIFFELIMVERBOSITY (N)
    (PROG (X)
      (SETQ X (REVAL1 (CAR N) T))
      (COND ((NULL (NUMBERP X)) (REDERR (LIST X " not a number"))))
      (SETQ *DIFFELIMVERBOSITY* (COND ((EQUAL X 0) NIL) (T X))))) 
(PUT 'DIFFELIMVERBOSITY 'STAT 'RLIS) 
(PUT 'GEN_DIFFELIM_INPUT 'NUMBER-OF-ARGS 3) 
(PUT 'GEN_DIFFELIM_INPUT 'DEFINED-ON-LINE '177) 
(PUT 'GEN_DIFFELIM_INPUT 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'GEN_DIFFELIM_INPUT 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE GEN_DIFFELIM_INPUT (U N V)
    (PROG (*ALLFAC *NAT DFPRINTFN_SAVED EXPTPRTCH_SAVED V1 SL DEPLS)
      (SETQ DFPRINTFN_SAVED (GET 'DF 'PRIFN))
      (PUT 'DF 'PRIFN 'DF_DIFFELIM_FORMAT)
      (SETQ EXPTPRTCH_SAVED (GET 'EXPT 'PRTCH))
      (PUT 'EXPT 'PRTCH '^)
      (MAPRIN "V := [ ")
      (TERPRI* T)
      (SETQ DEPLS DEPL*)
     F
      (SETQ V1 (EXPLODE (CAAR DEPLS)))
      (COND
       ((NULL (OR (EQNAME_P V1) (EQUAL (CAR (LASTPAIR V1)) '_)))
        (MAPRIN (CAR DEPLS)))
       (T (PROGN (SETQ DEPLS (CDR DEPLS)) (GO F))))
      (SETQ DEPLS (CDR DEPLS))
      (COND ((NULL DEPLS) (GO H)))
     G
      (SETQ V1 (EXPLODE (CAAR DEPLS)))
      (COND
       ((NULL (OR (EQNAME_P V1) (EQUAL (CAR (LASTPAIR V1)) '_)))
        (PROGN (MAPRIN ",") (TERPRI* T) (MAPRIN (CAR DEPLS)))))
      (COND ((SETQ DEPLS (CDR DEPLS)) (GO G)))
     H
      (MAPRIN " ];")
      (SETQ SL (MKALLFUNSTRS))
      (TERPRI* T)
      (MAPRIN "U := [ ")
      (TERPRI* T)
     A
      (SQPRINT (CAR U))
      (COND ((SETQ U (CDR U)) (PROGN (MAPRIN ",") (TERPRI* T) (GO A)))
            (T (MAPRIN " ];")))
      (TERPRI* T)
      (COND ((NULL N) (GO J)))
      (MAPRIN "N := [ ")
      (TERPRI* T)
     CN
      (SQPRINT (CAR N))
      (COND ((SETQ N (CDR N)) (PROGN (MAPRIN ",") (TERPRI* T) (GO CN)))
            (T (MAPRIN " ];")))
     J
      (COND ((NULL V) (GO B)))
      (TERPRI* T)
      (MAPRIN "Janet := [[")
     D
      (SETQ V1 (CAR V))
     C
      (MAPRIN (COND ((ATOM (CAR V1)) (CAR V1)) (T (CAAR V1))))
      (COND ((SETQ V1 (CDR V1)) (PROGN (MAPRIN ",") (GO C)))
            ((SETQ V (CDR V)) (PROGN (MAPRIN "],[") (GO D)))
            (T (MAPRIN "]];")))
     B
      (TERPRI* T)
      (PUT 'DF 'PRIFN DFPRINTFN_SAVED)
      (PUT 'EXPT 'PRTCH EXPTPRTCH_SAVED)
      (RETURN SL))) 
(PUT 'EQNAME_P 'NUMBER-OF-ARGS 1) 
(PUT 'EQNAME_P 'DEFINED-ON-LINE '228) 
(PUT 'EQNAME_P 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'EQNAME_P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EQNAME_P (U) (EQNAME_P0 U (EXPLODE EQNAME_))) 
(PUT 'EQNAME_P0 'NUMBER-OF-ARGS 2) 
(PUT 'EQNAME_P0 'DEFINED-ON-LINE '231) 
(PUT 'EQNAME_P0 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'EQNAME_P0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EQNAME_P0 (U V)
    (COND ((NULL V) T)
          ((AND (NULL (ATOM U)) (EQ (CAR U) (CAR V)))
           (EQNAME_P0 (CDR U) (CDR V)))
          (T NIL))) 
(PUT 'DF_DIFFELIM_FORMAT 'NUMBER-OF-ARGS 1) 
(PUT 'DF_DIFFELIM_FORMAT 'DEFINED-ON-LINE '236) 
(PUT 'DF_DIFFELIM_FORMAT 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'DF_DIFFELIM_FORMAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DF_DIFFELIM_FORMAT (U)
    (PROG (X Y V)
      (SETQ V (CADDR U))
      (SETQ X (SETQ Y (LIST "Diff" (CADR U) (SETQ V (CADDR U)))))
      (SETQ U (CDDDR U))
      (COND ((NULL U) (RETURN (MAPRIN X))))
      (SETQ Y (CDDR X))
     A
      (COND
       ((NUMBERP (CAR U))
        (PROG (N)
          (SETQ N 2)
         LAB
          (COND ((MINUSP (DIFFERENCE (CAR U) N)) (RETURN NIL)))
          (SETQ Y (CDR (RPLACD Y (CONS V NIL))))
          (SETQ N (PLUS2 N 1))
          (GO LAB)))
       (T (SETQ Y (CDR (RPLACD Y (CONS (SETQ V (CAR U)) NIL))))))
      (COND ((SETQ U (CDR U)) (GO A)))
      (RETURN (MAPRIN X)))) 
(PUT 'READ_DIFFELIM_OUTPUT 'NUMBER-OF-ARGS 0) 
(PUT 'READ_DIFFELIM_OUTPUT 'DEFINED-ON-LINE '254) 
(PUT 'READ_DIFFELIM_OUTPUT 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'READ_DIFFELIM_OUTPUT 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE READ_DIFFELIM_OUTPUT NIL
    (PROG (X Y Z G OT N U B NSP *ECHO)
      (PUT 'DIFF 'NEWNAM 'DIFFELIMDIFF)
     A
      (SETQ X (SCAN))
      (SCAN)
      (SETQ Y
              (COND ((EQ (SETQ Z (SCAN)) '*LSQBKT*) (XREADDIFFELIMLIST))
                    (T (PROGN (SCAN) Z))))
      (COND ((EQ X 'V) NIL) ((EQ X 'G) (SETQ G Y)) ((EQ X 'OT) (SETQ OT Y))
            ((EQ X 'N) (SETQ N Y)) ((EQ X 'U) (SETQ U Y))
            ((EQ X 'B) (SETQ B Y)) ((EQ X 'NSP) (SETQ NSP Y))
            (T (REDERR "unknown result in DiffElim")))
      (COND ((NULL (EQ NXTSYM* 'END)) (GO A)))
      (SCAN)
      (SCAN)
      (REMPROP 'DIFF 'NEWNAM)
      (PUT 'DIFFELIMDIFF 'SIMPFN 'SIMPDIFFELIMDIFF)
      (COND
       (G
        (SETQ G
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J G)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND
       (OT
        (SETQ OT
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J OT)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND
       (N
        (SETQ N
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J N)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND
       (U
        (SETQ U
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J U)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (COND
       (B
        (SETQ B
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J B)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))
      (SETQ NSP (COND (NSP NSP) (T 0)))
      (REMPROP 'DIFFELIMDIFF 'SIMPFN)
      (RETURN (LIST G OT N U B NSP)))) 
(PUT 'SIMPDIFFELIMDIFF 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPDIFFELIMDIFF 'DEFINED-ON-LINE '288) 
(PUT 'SIMPDIFFELIMDIFF 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'SIMPDIFFELIMDIFF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPDIFFELIMDIFF (U)
    (PROG (W)
      (RETURN
       (COND
        ((AND (SETQ W (ASSOC (CAAR U) DEPL*))
              (NULL (SETDIFF (CDR W) (CDAR U))))
         (SIMP (CONS 'DF (CONS (CAAR U) (CDR U)))))
        (T (SIMP (CONS 'DF U))))))) 
(PUT 'XREADDIFFELIMLIST 'NUMBER-OF-ARGS 0) 
(PUT 'XREADDIFFELIMLIST 'DEFINED-ON-LINE '296) 
(PUT 'XREADDIFFELIMLIST 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'XREADDIFFELIMLIST 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE XREADDIFFELIMLIST NIL
    (PROG (CURSYM DELIM LST)
      (COND ((EQ (SCAN) '*RSQBKT*) (PROGN (SCAN) (RETURN NIL))))
     A
      (SETQ LST (ACONC LST (XREAD1 'GROUP)))
      (SETQ CURSYM CURSYM*)
      (COND
       ((EQ CURSYM '*SEMICOL*) (SYMERR "Syntax error: semicolon in list" NIL))
       ((AND (EQ (SCAN) '*RSQBKT*) (EQ CURSYM '*COMMA*))
        (SYMERR "Syntax error: invalid comma in list" NIL)))
      (COND ((EQ CURSYM '*RSQBKT*) (RETURN LST))
            ((NULL DELIM) (SETQ DELIM CURSYM)))
      (GO A))) 
(PUT 'MKALLFUNSTRS 'NUMBER-OF-ARGS 0) 
(PUT 'MKALLFUNSTRS 'DEFINED-ON-LINE '315) 
(PUT 'MKALLFUNSTRS 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'MKALLFUNSTRS 'PROCEDURE_TYPE '(ARROW UNIT GENERAL)) 
(DE MKALLFUNSTRS NIL
    (PROG (IX X S DEPLS FNSL)
      (COND ((NULL DEPL*) (RETURN NIL)))
      (SETQ DEPLS DEPL*)
     B
      (SETQ X (CAR DEPLS))
      (SETQ IX X)
      (SETQ S (BLDMSG_INTERNAL "%p%w" (LIST (CAR X) "()")))
      (SETQ FNSL (CONS (CONS (LIST (CAR IX)) (GET (CAR IX) 'OLDNAM)) FNSL))
      (PUT (CAR IX) 'OLDNAM S)
      (SETQ DEPLS (CDR DEPLS))
      (COND (DEPLS (GO B)))
      (RETURN FNSL))) 
(PUT 'UNMKALLFUNSTRS 'NUMBER-OF-ARGS 1) 
(PUT 'UNMKALLFUNSTRS 'DEFINED-ON-LINE '329) 
(PUT 'UNMKALLFUNSTRS 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'UNMKALLFUNSTRS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNMKALLFUNSTRS (U)
    (PROG (J)
      (SETQ J U)
     LAB
      (COND ((NULL J) (RETURN NIL)))
      ((LAMBDA (J)
         (COND ((CDR J) (PUT (CAAR J) 'OLDNAM (CDR J)))
               (T (REMPROP (CAAR J) 'OLDNAM))))
       (CAR J))
      (SETQ J (CDR J))
      (GO LAB))) 
(PUT 'UNARGFUNS 'NUMBER-OF-ARGS 1) 
(PUT 'UNARGFUNS 'DEFINED-ON-LINE '334) 
(PUT 'UNARGFUNS 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'UNARGFUNS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE UNARGFUNS (U)
    (PROG (J)
      (SETQ J U)
     LAB
      (COND ((NULL J) (RETURN NIL)))
      ((LAMBDA (J) (SETK (CAR J) (REVAL1 (CAAR J) NIL))) (CAR J))
      (SETQ J (CDR J))
      (GO LAB))) 
(PUT 'CLEAR_UNARGFUNS 'NUMBER-OF-ARGS 1) 
(PUT 'CLEAR_UNARGFUNS 'DEFINED-ON-LINE '337) 
(PUT 'CLEAR_UNARGFUNS 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'CLEAR_UNARGFUNS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLEAR_UNARGFUNS (U)
    (PROG (J)
      (SETQ J U)
     LAB
      (COND ((NULL J) (RETURN NIL)))
      ((LAMBDA (J)
         (PROGN
          (REMPROP (CAAR J) 'SIMPFN)
          (REMKLIST (CAAR J))
          (REMPROP (CAAR J) 'KVALUE)))
       (CAR J))
      (SETQ J (CDR J))
      (GO LAB))) 
(PUT 'DIFF_ELIM 'NUMBER-OF-ARGS 1) 
(PUT 'DIFF_ELIM 'DEFINED-ON-LINE '343) 
(PUT 'DIFF_ELIM 'DEFINED-IN-FILE 'CRACK/CRDIFFELIM.RED) 
(PUT 'DIFF_ELIM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DIFF_ELIM (U)
    (PROG (X Y Z N RES)
      (COND
       ((NULL (EQ (GETRTYPE (SETQ X (REVAL1 (CAR U) NIL))) 'LIST))
        (TYPERR X "list")))
      (COND
       ((NULL (EQ (GETRTYPE (SETQ Y (REVAL1 (CADR U) T))) 'LIST))
        (TYPERR Y "list")))
      (COND
       ((NULL (EQ (GETRTYPE (SETQ N (REVAL1 (CADDR U) T))) 'LIST))
        (TYPERR N "list")))
      (SETQ Z (COND ((CDDDR U) (REVAL1 (CADDDR U) T))))
      (COND
       ((AND Z (NULL (NUMBERP Z)))
        (REDERR "Fourth argument must be a number")))
      (SETQ RES
              (CALL_DIFFELIM
               (PROG (J FORALL-RESULT FORALL-ENDPTR)
                 (SETQ J (CDR X))
                 (COND ((NULL J) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL)))
                LOOPLABEL
                 (SETQ J (CDR J))
                 (COND ((NULL J) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (COND ((NULL (CDR Y)) NIL)
                     (T
                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                        (SETQ J (CDR Y))
                        (COND ((NULL J) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (J) (CDR J)) (CAR J))
                                              NIL)))
                       LOOPLABEL
                        (SETQ J (CDR J))
                        (COND ((NULL J) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CDR J)) (CAR J)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
               (COND ((NULL (CDR N)) NIL)
                     (T
                      (PROG (J FORALL-RESULT FORALL-ENDPTR)
                        (SETQ J (CDR N))
                        (COND ((NULL J) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (J) (SIMP J)) (CAR J))
                                              NIL)))
                       LOOPLABEL
                        (SETQ J (CDR J))
                        (COND ((NULL J) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (SIMP J)) (CAR J)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
               Z))
      (RETURN
       (LIST 'LIST
             (CONS 'LIST
                   (PROG (J FORALL-RESULT FORALL-ENDPTR)
                     (SETQ J (CAR RES))
                     (COND ((NULL J) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                           NIL)))
                    LOOPLABEL
                     (SETQ J (CDR J))
                     (COND ((NULL J) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J)) NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL)))
             (CONS 'LIST
                   (COND ((NULL (CADR RES)) NIL)
                         (T
                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                            (SETQ J (CADR RES))
                            (COND ((NULL J) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                             NIL)))
                           LOOPLABEL
                            (SETQ J (CDR J))
                            (COND ((NULL J) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
             (CONS 'LIST
                   (COND ((NULL (CADDR RES)) NIL)
                         (T
                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                            (SETQ J (CADDR RES))
                            (COND ((NULL J) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                             NIL)))
                           LOOPLABEL
                            (SETQ J (CDR J))
                            (COND ((NULL J) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
             (CONS 'LIST
                   (COND ((NULL (CADDDR RES)) NIL)
                         (T
                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                            (SETQ J (CADDDR RES))
                            (COND ((NULL J) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                             NIL)))
                           LOOPLABEL
                            (SETQ J (CDR J))
                            (COND ((NULL J) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
             (CONS 'LIST
                   (COND ((NULL (CAR (CDDDDR RES))) NIL)
                         (T
                          (PROG (J FORALL-RESULT FORALL-ENDPTR)
                            (SETQ J (CAR (CDDDDR RES)))
                            (COND ((NULL J) (RETURN NIL)))
                            (SETQ FORALL-RESULT
                                    (SETQ FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                             NIL)))
                           LOOPLABEL
                            (SETQ J (CDR J))
                            (COND ((NULL J) (RETURN FORALL-RESULT)))
                            (RPLACD FORALL-ENDPTR
                                    (CONS ((LAMBDA (J) (MK*SQ J)) (CAR J))
                                          NIL))
                            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                            (GO LOOPLABEL)))))
             (CADR (CDDDDR RES)))))) 
(PUT 'DIFFELIM 'PSOPFN 'DIFF_ELIM) 
(ENDMODULE) 