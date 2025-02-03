(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'RLSERVICE)) 
(REVISION 'RLSERVICE
          "$Id: rlservice.red 6030 2021-09-16 14:01:45Z thomas-sturm $") 
(COPYRIGHT 'RLSERVICE "(c) 2016-2020 T. Sturm") 
(FLUID '(*LOWER)) 
(FLUID '(*RAISE)) 
(FLUID '(MODE*)) 
(GLOBAL '(RL_SERVICES*)) 
(FLUID '(RL_SERVL*)) 
(PUT 'RL_SERVICE 'STAT 'RL_SERVICESTAT) 
(PUT 'RL_SERVICESTAT 'NUMBER-OF-ARGS 0) 
(DE RL_SERVICESTAT NIL
    (PROG (SPEC)
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* '*LCBKT*)
         (REDERR
          (LIST "expecting '{' in, " "rl_service" "but found" CURSYM*)))))
      (SETQ SPEC (RL_SERVICESTATLIST))
      (SCAN)
      (RETURN (LIST 'RL_SERVICE SPEC)))) 
(PUT 'RL_SERVICESTATLIST 'NUMBER-OF-ARGS 0) 
(DE RL_SERVICESTATLIST NIL
    (PROG (SPEC KEY ENTRY)
      (SCAN)
      (PROG ()
       WHILELABEL
        (COND ((NOT (NEQ CURSYM* '*RCBKT*)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (LTO_ALPHAP CURSYM*))
           (REDERR
            (LIST "expecting alphabetic key in rl_service but found"
                  CURSYM*))))
         (SETQ KEY CURSYM*)
         (SETQ ENTRY
                 (COND ((EQ KEY 'DEFAULT) (RL_AMREADDEFAULTTOFORM))
                       ((EQ KEY 'TYPE) (RL_CSREADTYPETOSTRING))
                       (T (RL_READLISTORATOM))))
         (PROG (W1)
           (SETQ W1 (CONS KEY ENTRY))
           (SETQ SPEC (CONS W1 SPEC))
           (RETURN W1))
         (COND
          ((NEQ CURSYM* '*RCBKT*)
           (PROGN
            (COND
             ((NEQ CURSYM* '*COMMA*)
              (REDERR
               (LIST "expecting ',' or '}' in rl_service but found" CURSYM*))))
            (SCAN)))))
        (GO WHILELABEL))
      (RETURN (REVERSIP SPEC)))) 
(PUT 'RL_AMREADDEFAULTTOFORM 'NUMBER-OF-ARGS 0) 
(DE RL_AMREADDEFAULTTOFORM NIL
    (PROG (*MODE)
      (SETQ *MODE (PROGN (SETQ ALGLIST* (CONS NIL NIL)) 'ALGEBRAIC))
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* 'EQUAL)
         (REDERR (LIST "expecting '=' in" "rl_service" "but found" CURSYM*)))))
      (RETURN (XREAD T)))) 
(PUT 'RL_CSREADTYPETOSTRING 'NUMBER-OF-ARGS 0) 
(DE RL_CSREADTYPETOSTRING NIL
    (PROG (*LOWER *RAISE)
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* 'EQUAL)
         (REDERR (LIST "expecting '=' in" "rl_service" "but found" CURSYM*)))))
      (RETURN (IOTO_SMAPRIN (XREAD T))))) 
(PUT 'RL_READLISTORATOM 'NUMBER-OF-ARGS 0) 
(DE RL_READLISTORATOM NIL
    (PROG (ENTRY)
      (PROGN
       (SCAN)
       (COND
        ((NEQ CURSYM* 'EQUAL)
         (REDERR (LIST "expecting '=' in" "rl_service" "but found" CURSYM*)))))
      (SCAN)
      (COND ((EQ CURSYM* '*LCBKT*) (PROGN (SETQ ENTRY (RL_SERVICESTATLIST))))
            (T
             (PROGN
              (COND
               ((NOT (ATOM CURSYM*))
                (REDERR
                 (LIST "expecting atomic entry or list in rl_service but found"
                       CURSYM*))))
              (SETQ ENTRY CURSYM*))))
      (SCAN)
      (RETURN ENTRY))) 
(PUT 'RL_SERVICE 'FORMFN 'RL_FORMSERVICE) 
(PUT 'RL_FORMSERVICE 'NUMBER-OF-ARGS 3) 
(DE RL_FORMSERVICE (ARGL VARS M)
    (PROG (MODE)
      (SETQ MODE (LTO_EATSOC 'MODE (CADR ARGL) (LIST "missing mode in" ARGL)))
      (COND ((EQ MODE 'BOTH) (RETURN (RL_FORMSERVICEBOTH (CADR ARGL)))))
      (COND ((EQ MODE 'SM) (RETURN (RL_FORMSERVICESM (CADR ARGL)))))
      (REDERR (LIST "invalid mode" MODE "in" ARGL)))) 
(PUT 'RL_FORMSERVICEBOTH 'NUMBER-OF-ARGS 1) 
(DE RL_FORMSERVICEBOTH (SPEC)
    (PROG (B DOC SEEALSO NAMES TYPES DEFAULTS DOCS RTYPE RL_ARGS RL_*ARGS RL_B*
           RL_B RL_*B RL_B$ RLB P DOCAL FLUIDS)
      (PROG (G131 G132)
        (SETQ G131 (RL_FORMSERVICEANALYZESPEC SPEC))
        (SETQ G132 G131)
        (SETQ B (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ DOC (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ SEEALSO (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ NAMES (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ TYPES (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ DEFAULTS (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ DOCS (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ RTYPE (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ RL_ARGS (CAR G131))
        (SETQ G131 (CDR G131))
        (SETQ RL_*ARGS (CAR G131))
        (SETQ G131 (CDR G131))
        (RETURN G132))
      (PROG (G133 G134)
        (SETQ G133 (RL_FORMSERVICEFUNCTIONNAMES 'RL_ B))
        (SETQ G134 G133)
        (SETQ RL_B* (CAR G133))
        (SETQ G133 (CDR G133))
        (SETQ RL_B (CAR G133))
        (SETQ G133 (CDR G133))
        (SETQ RL_*B (CAR G133))
        (SETQ G133 (CDR G133))
        (SETQ RL_B$ (CAR G133))
        (SETQ G133 (CDR G133))
        (SETQ RLB (CAR G133))
        (SETQ G133 (CDR G133))
        (RETURN G134))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''RTYPEFN ''RTYPEPART))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''RL_SUPPORT ''RL_AMSERVICE))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''RL_SMSERVICE (MKQUOTE RL_B)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''NAMES (MKQUOTE NAMES)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''INTYPES (MKQUOTE TYPES)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''DEFAULTS (MKQUOTE DEFAULTS)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''OUTTYPE RTYPE))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''DESCRIPTION DOC))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (COND
       (SEEALSO
        (PROG (W1)
          (SETQ W1
                  (LIST 'PUT (MKQUOTE RLB) ''SEEALSO
                        (MKQUOTE
                         (PROG (S FORALL-RESULT FORALL-ENDPTR)
                           (SETQ S SEEALSO)
                           (COND ((NULL S) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (S)
                                               (COMPRESS
                                                (CONS 'R
                                                      (CONS 'L (EXPLODE S)))))
                                             (CAR S))
                                            NIL)))
                          LOOPLABEL
                           (SETQ S (CDR S))
                           (COND ((NULL S) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (S)
                                       (COMPRESS
                                        (CONS 'R (CONS 'L (EXPLODE S)))))
                                     (CAR S))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
          (SETQ P (CONS W1 P))
          (RETURN W1))))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''DOCS (MKQUOTE DOCS)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B) ''RL_AMSERVICE (MKQUOTE RLB)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RLB) ''PSOPFN (MKQUOTE RL_B$)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B$) ''CLEANUPFN ''RL_CLEANUP))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B$) ''NUMBER-OF-ARGS 1))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1
                (LIST 'DE RL_B$ '(U)
                      (LIST 'RL_SERVICEWRAPPER (MKQUOTE RL_*B) 'U
                            (MKQUOTE NAMES)
                            (MKQUOTE
                             (PROG (X FORALL-RESULT FORALL-ENDPTR)
                               (SETQ X TYPES)
                               (COND ((NULL X) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       (SETQ FORALL-ENDPTR
                                               (CONS
                                                ((LAMBDA (X)
                                                   (RL_TYPESTRING2TYPEFORM X))
                                                 (CAR X))
                                                NIL)))
                              LOOPLABEL
                               (SETQ X (CDR X))
                               (COND ((NULL X) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X)
                                           (RL_TYPESTRING2TYPEFORM X))
                                         (CAR X))
                                        NIL))
                               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                               (GO LOOPLABEL)))
                            (MKQUOTE DEFAULTS)
                            (MKQUOTE (RL_TYPESTRING2TYPEFORM RTYPE))
                            (MKQUOTE RL_B*) (MKQUOTE RLB))))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (COND
       (FLUIDS
        (PROG (W1)
          (SETQ W1 (LIST 'FLUID (MKQUOTE FLUIDS)))
          (SETQ P (CONS W1 P))
          (RETURN W1))))
      (PROG (W1)
        (SETQ W1
                (LIST 'PUT (MKQUOTE RL_*B) ''NUMBER-OF-ARGS (LENGTH RL_*ARGS)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1
                (LIST 'DE RL_*B RL_*ARGS
                      (LIST 'APPLY (MKQUOTE RL_B) (CONS 'LIST RL_ARGS))))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (SETQ P (RL_FORMSERVICESM1 RL_B RL_B* RL_ARGS P))
      (RETURN (CONS 'PROGN (REVERSIP P))))) 
(PUT 'RL_FORMSERVICEANALYZESPEC 'NUMBER-OF-ARGS 1) 
(DE RL_FORMSERVICEANALYZESPEC (SPEC)
    (PROG (B DOC SEEALSO NAMES TYPES DEFAULTS DOCS RTYPE RL_ARGS RL_*ARGS
           DEFAULT POS NAME TYPE MINSWITCH MAXOTHER MAXALL)
      (SETQ MINSWITCH 0)
      (SETQ MAXOTHER 0)
      (SETQ MAXALL 0)
      (SETQ B (LTO_EATSOC 'NAME SPEC (LIST "missing service name in" SPEC)))
      (SETQ MINSWITCH (LENGTH SPEC))
      (PROG (PR)
        (SETQ PR SPEC)
       LAB
        (COND ((NULL PR) (RETURN NIL)))
        ((LAMBDA (PR)
           (COND
            ((EQ (CAR PR) 'ARG)
             (PROGN
              (SETQ NAME
                      (LTO_EATSOC 'NAME (CDR PR)
                                  (LIST "arg without name in service" B)))
              (SETQ POS
                      (LTO_EATSOC 'POS (CDR PR)
                                  (LIST "arg" NAME "without pos in service"
                                        B)))
              (COND
               ((ASSOC POS NAMES)
                (REDERR (LIST "pos" POS "specified twice in service" B))))
              (SETQ DOC (OR (LTO_CATSOC 'DOC (CDR PR)) ""))
              (SETQ TYPE
                      (LTO_EATSOC 'TYPE (CDR PR)
                                  (LIST "arg" NAME "without type in service"
                                        B)))
              (COND
               ((EQUAL TYPE "Switch")
                (PROGN
                 (SETQ MINSWITCH (MIN2 POS MINSWITCH))
                 (SETQ MAXALL (MAX2 POS MAXALL))
                 (SETQ DEFAULT
                         (INTERN
                          (COMPRESS (APPEND (EXPLODE '*RL) (EXPLODE NAME)))))
                 (PROG (W1)
                   (SETQ W1 (CONS POS (CONS NAME DEFAULT)))
                   (SETQ DEFAULTS (CONS W1 DEFAULTS))
                   (RETURN W1))
                 (PROG (W1)
                   (SETQ W1 (CONS POS DEFAULT))
                   (SETQ RL_*ARGS (CONS W1 RL_*ARGS))
                   (RETURN W1))))
               (T
                (PROGN
                 (SETQ MAXOTHER (MAX2 POS MAXOTHER))
                 (SETQ MAXALL (MAX2 POS MAXALL))
                 (SETQ DEFAULT (ATSOC 'DEFAULT (CDR PR)))
                 (COND
                  (DEFAULT
                   (PROG (W1)
                     (SETQ W1 (CONS POS (CONS NAME (CDR DEFAULT))))
                     (SETQ DEFAULTS (CONS W1 DEFAULTS))
                     (RETURN W1))))
                 (PROG (W1)
                   (SETQ W1 (CONS POS NAME))
                   (SETQ RL_ARGS (CONS W1 RL_ARGS))
                   (RETURN W1))
                 (PROG (W1)
                   (SETQ W1 (CONS POS NAME))
                   (SETQ RL_*ARGS (CONS W1 RL_*ARGS))
                   (RETURN W1)))))
              (PROG (W1)
                (SETQ W1 (CONS POS NAME))
                (SETQ NAMES (CONS W1 NAMES))
                (RETURN W1))
              (PROG (W1)
                (SETQ W1 (CONS POS DOC))
                (SETQ DOCS (CONS W1 DOCS))
                (RETURN W1))
              (PROG (W1)
                (SETQ W1 (CONS POS TYPE))
                (SETQ TYPES (CONS W1 TYPES))
                (RETURN W1))))
            ((EQ (CAR PR) 'RETURNS)
             (PROGN
              (SETQ RTYPE
                      (LTO_EATSOC 'TYPE (CDR PR)
                                  (LIST "service" B "without return type")))))
            ((EQ (CAR PR) 'SEEALSO)
             (PROGN
              (PROG (W1)
                (SETQ W1 (CDR PR))
                (SETQ SEEALSO (CONS W1 SEEALSO))
                (RETURN W1))))))
         (CAR PR))
        (SETQ PR (CDR PR))
        (GO LAB))
      (COND
       ((NOT (EQN MAXALL (LENGTH NAMES)))
        (REDERR (LIST "bad arg position numbering in service" B))))
      (COND
       ((AND (NEQ MINSWITCH (LENGTH SPEC)) (NEQ MINSWITCH (PLUS MAXOTHER 1)))
        (REDERR (LIST "bad switch positions in service" B))))
      (SETQ NAMES (RL_SORTANDPROJECT NAMES))
      (SETQ DOCS (RL_SORTANDPROJECT DOCS))
      (SETQ TYPES (RL_SORTANDPROJECT TYPES))
      (SETQ RL_ARGS (RL_SORTANDPROJECT RL_ARGS))
      (SETQ RL_*ARGS (RL_SORTANDPROJECT RL_*ARGS))
      (COND
       ((GREATERP (LENGTH RL_*ARGS) 14)
        (REDERR (LIST "too many arguments for service" B))))
      (SETQ DEFAULTS (RL_SORTANDPROJECT DEFAULTS))
      (SETQ DOC (OR (LTO_CATSOC 'DOC SPEC) ""))
      (SETQ SEEALSO (SORT SEEALSO (FUNCTION ORDP)))
      (RETURN
       (LIST B DOC SEEALSO NAMES TYPES DEFAULTS DOCS RTYPE RL_ARGS RL_*ARGS)))) 
(PUT 'RL_FORMSERVICEFUNCTIONNAMES 'NUMBER-OF-ARGS 2) 
(DE RL_FORMSERVICEFUNCTIONNAMES (RL_ B)
    (PROG (RL RL_B RL_*B RLB RL_B$ RL_B*)
      (SETQ RL_ (REVERSIP (EXPLODE RL_)))
      (SETQ RL (REVERSE (CDR RL_)))
      (SETQ RL_ (REVERSIP RL_))
      (SETQ RL_B (INTERN (COMPRESS (APPEND RL_ (EXPLODE B)))))
      (SETQ RL_*B
              (INTERN
               (COMPRESS (APPEND RL_ (APPEND (EXPLODE '*) (EXPLODE B))))))
      (SETQ RLB (INTERN (COMPRESS (APPEND RL (EXPLODE B)))))
      (SETQ RL_B$ (INTERN (COMPRESS (NCONC (EXPLODE RL_B) '(! $)))))
      (SETQ RL_B* (INTERN (COMPRESS (NCONC (EXPLODE RL_B) '(! *)))))
      (RETURN (LIST RL_B* RL_B RL_*B RL_B$ RLB)))) 
(PUT 'RL_TYPESTRING2TYPEFORM 'NUMBER-OF-ARGS 1) 
(DE RL_TYPESTRING2TYPEFORM (S)
    (PROG (X)
      ((LAMBDA (*LOWER *RAISE) (SETQ X (IOTO_SXREAD S))) NIL NIL)
      (COND ((IDP X) (RETURN (INTERN (LTO_DOWNCASE X)))))
      (RETURN
       (PROG (Y FORALL-RESULT FORALL-ENDPTR)
         (SETQ Y X)
         (COND ((NULL Y) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (Y) (LTO_DOWNCASE Y)) (CAR Y)) NIL)))
        LOOPLABEL
         (SETQ Y (CDR Y))
         (COND ((NULL Y) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (Y) (LTO_DOWNCASE Y)) (CAR Y)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'RL_FORMSERVICESM 'NUMBER-OF-ARGS 1) 
(DE RL_FORMSERVICESM (SPEC)
    (PROG (B RL_B RL_B* ARGL SL DOCAL P N)
      (SETQ N 0)
      (SETQ B (LTO_EATSOC 'NAME SPEC (LIST "missing service name in" SPEC)))
      (SETQ RL_B (INTERN (COMPRESS (NCONC (EXPLODE 'RL_) (EXPLODE B)))))
      (SETQ RL_B* (INTERN (COMPRESS (NCONC (EXPLODE RL_B) '(! *)))))
      (SETQ N (LTO_EATSOC 'ARGNUM SPEC (LIST "missing argnum in" SPEC)))
      (PROG (W1)
        (SETQ W1 (LTO_AT2STR RL_B))
        (SETQ SL (CONS W1 SL))
        (RETURN W1))
      (PROGN (SETQ SL (CONS "/" SL)) "/")
      (PROG (W1)
        (SETQ W1 (LTO_AT2STR (LTO_INT2ID N)))
        (SETQ SL (CONS W1 SL))
        (RETURN W1))
      (SETQ DOCAL
              (LIST (CONS 'SYNOPSIS (LTO_SCONCAT (REVERSIP SL)))
                    (OR (CONS 'DESCRIPTION (LTO_CATSOC 'DOC SPEC)) "")))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B) ''DOCAL (MKQUOTE DOCAL)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (SETQ ARGL
              (PROG (I FORALL-RESULT FORALL-ENDPTR)
                (SETQ I 1)
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR (CONS (MKID 'A I) NIL)))
               LOOPLABEL
                (SETQ I (PLUS2 I 1))
                (COND ((MINUSP (DIFFERENCE N I)) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR (CONS (MKID 'A I) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ P (RL_FORMSERVICESM1 RL_B RL_B* ARGL P))
      (RETURN (CONS 'PROGN (REVERSIP P))))) 
(PUT 'RL_FORMSERVICESM1 'NUMBER-OF-ARGS 4) 
(DE RL_FORMSERVICESM1 (RL_B RL_B* ARGL P)
    (PROG ()
      (PROG (W1)
        (SETQ W1 (LIST 'FLUID (MKQUOTE (LIST RL_B*))))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1
                (LIST 'SETQ 'RL_SERVL*
                      (LIST 'CONS (MKQUOTE RL_B*) 'RL_SERVL*)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B) ''RL_SUPPORT ''RL_SMSERVICE))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1
                (LIST 'SETQ 'RL_SERVICES*
                      (LIST 'CONS (MKQUOTE RL_B) 'RL_SERVICES*)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'PUT (MKQUOTE RL_B) ''NUMBER-OF-ARGS (LENGTH ARGL)))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (PROG (W1)
        (SETQ W1 (LIST 'DE RL_B ARGL (LIST 'APPLY RL_B* (CONS 'LIST ARGL))))
        (SETQ P (CONS W1 P))
        (RETURN W1))
      (RETURN P))) 
(PUT 'RL_SORTANDPROJECT 'NUMBER-OF-ARGS 1) 
(DE RL_SORTANDPROJECT (AL)
    (PROG (PR FORALL-RESULT FORALL-ENDPTR)
      (SETQ PR (SORT AL (FUNCTION (LAMBDA (X Y) (LESSP (CAR X) (CAR Y))))))
      (COND ((NULL PR) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS ((LAMBDA (PR) (CDR PR)) (CAR PR)) NIL)))
     LOOPLABEL
      (SETQ PR (CDR PR))
      (COND ((NULL PR) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (PR) (CDR PR)) (CAR PR)) NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'RL_DOCSYNOPSIS 'NUMBER-OF-ARGS 4) 
(DE RL_DOCSYNOPSIS (F NAMES TYPES DEFAULTS)
    (PROG (SL NAME DEFAULT)
      (PROG (W1) (SETQ W1 (ID2STRING F)) (SETQ SL (CONS W1 SL)) (RETURN W1))
      (PROGN (SETQ SL (CONS "(" SL)) "(")
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TYPES (NEQ (CAR TYPES) "Switch"))) (RETURN NIL)))
        (PROGN
         (SETQ NAME (PROG1 (CAR NAMES) (SETQ NAMES (CDR NAMES))))
         (PROG (W1)
           (SETQ W1 (ID2STRING NAME))
           (SETQ SL (CONS W1 SL))
           (RETURN W1))
         (SETQ DEFAULT (ATSOC NAME DEFAULTS))
         (COND
          (DEFAULT
           (PROGN
            (PROGN (SETQ SL (CONS " = " SL)) " = ")
            (COND
             ((STRINGP (CDR DEFAULT))
              (PROGN
               (PROGN (SETQ SL (CONS "\"" SL)) "\"")
               (PROG (W1)
                 (SETQ W1 (CDR DEFAULT))
                 (SETQ SL (CONS W1 SL))
                 (RETURN W1))
               (PROGN (SETQ SL (CONS "\"" SL)) "\"")))
             (T
              (PROG (W1)
                (SETQ W1 (IOTO_SMAPRIN (CDR DEFAULT)))
                (SETQ SL (CONS W1 SL))
                (RETURN W1)))))))
         (PROGN (SETQ SL (CONS ": " SL)) ": ")
         (PROG (W1)
           (SETQ W1 (PROG1 (CAR TYPES) (SETQ TYPES (CDR TYPES))))
           (SETQ SL (CONS W1 SL))
           (RETURN W1))
         (COND (TYPES (PROGN (SETQ SL (CONS ", " SL)) ", "))))
        (GO WHILELABEL))
      (COND (TYPES (PROGN (SETQ SL (CONS "..." SL)) "...")))
      (PROGN (SETQ SL (CONS ")" SL)) ")")
      (RETURN (LTO_SCONCAT (REVERSIP SL))))) 
(PUT 'RL_DOCARGUMENTS 'NUMBER-OF-ARGS 3) 
(DE RL_DOCARGUMENTS (NAMES TYPES DOCS)
    (PROG (SL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TYPES (NEQ (CAR TYPES) "Switch"))) (RETURN NIL)))
        (PROGN
         (PROG1 (CAR TYPES) (SETQ TYPES (CDR TYPES)))
         (PROG (W1)
           (SETQ W1
                   (CONS
                    (ID2STRING (PROG1 (CAR NAMES) (SETQ NAMES (CDR NAMES))))
                    (PROG1 (CAR DOCS) (SETQ DOCS (CDR DOCS)))))
           (SETQ SL (CONS W1 SL))
           (RETURN W1)))
        (GO WHILELABEL))
      (RETURN (REVERSIP SL)))) 
(PUT 'RL_DOCSWITCHES 'NUMBER-OF-ARGS 3) 
(DE RL_DOCSWITCHES (NAMES TYPES DOCS)
    (PROG (SL)
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND TYPES (NEQ (CAR TYPES) "Switch"))) (RETURN NIL)))
        (PROGN
         (PROG1 (CAR TYPES) (SETQ TYPES (CDR TYPES)))
         (PROG1 (CAR NAMES) (SETQ NAMES (CDR NAMES)))
         (PROG1 (CAR DOCS) (SETQ DOCS (CDR DOCS))))
        (GO WHILELABEL))
      (PROG ()
       WHILELABEL
        (COND ((NOT NAMES) (RETURN NIL)))
        (PROG (W1)
          (SETQ W1
                  (CONS
                   (ID2STRING (PROG1 (CAR NAMES) (SETQ NAMES (CDR NAMES))))
                   (PROG1 (CAR DOCS) (SETQ DOCS (CDR DOCS)))))
          (SETQ SL (CONS W1 SL))
          (RETURN W1))
        (GO WHILELABEL))
      (RETURN SL))) 
(PUT 'RL_SERVICEWRAPPER 'NUMBER-OF-ARGS 8) 
(DE RL_SERVICEWRAPPER (RL_*B U NAMES TYPES DEFAULTS RTYPE RL_B* RLB)
    (PROG (G RARGS NARGS W NAME ARGC POS)
      (SETQ ARGC 0)
      (SETQ POS 0)
      (COND
       ((NULL (EVAL RL_B*))
        (REDERR
         (LIST "service" RLB "not available in current context" RL_CID*))))
      (SETQ ARGC (LENGTH NAMES))
      (SETQ G (GENSYM))
      (PROG (I)
        (SETQ I 1)
       LAB
        (COND ((MINUSP (DIFFERENCE ARGC I)) (RETURN NIL)))
        (PROGN (SETQ RARGS (CONS G RARGS)) G)
        (SETQ I (PLUS2 I 1))
        (GO LAB))
      (PROG (ARG)
        (SETQ ARG U)
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG)
           (PROGN
            (SETQ POS (PLUS POS 1))
            (COND
             ((OR (EQCAR ARG 'REPLACEBY)
                  (AND (EQCAR ARG 'EQUAL)
                       (NOT (RL_TYPEEQUATIONALP (NTH TYPES POS)))))
              (PROG (W1)
                (SETQ W1 (CONS (CADR ARG) (CADDR ARG)))
                (SETQ NARGS (CONS W1 NARGS))
                (RETURN W1)))
             (T (SETCAR (PNTH RARGS POS) ARG)))))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (PROG (ARG)
        (SETQ ARG NARGS)
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        ((LAMBDA (ARG)
           (PROGN
            (SETQ W (MEMQ (CAR ARG) NAMES))
            (COND
             ((NOT W) (REDERR (LIST "unknown named parameter" (CAR ARG)))))
            (SETQ POS (PLUS (DIFFERENCE ARGC (LENGTH W)) 1))
            (COND
             ((NEQ (NTH RARGS POS) G)
              (REDERR
               (LIST "ambiguous specification for parameter" (CAR ARG)))))
            (SETCAR (PNTH RARGS POS) (CDR ARG))))
         (CAR ARG))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (SETQ POS 0)
      (PROG (ARG)
        (SETQ ARG RARGS)
       LAB
        (COND ((NULL ARG) (RETURN NIL)))
        (PROGN
         (SETQ POS (PLUS POS 1))
         (COND
          ((EQ (CAR ARG) G)
           (PROGN
            (SETQ NAME (NTH NAMES POS))
            (SETQ W (ATSOC NAME DEFAULTS))
            (COND
             ((NOT W)
              (REDERR (LIST "missing parameter" NAME "at position" POS))))
            (SETCAR ARG (CDR W))))))
        (SETQ ARG (CDR ARG))
        (GO LAB))
      (SETQ RARGS
              (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                (SETQ ARG RARGS)
                (COND ((NULL ARG) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ARG)
                                    (RL_CONVERTARG ARG
                                     (PROG1 (CAR TYPES)
                                       (SETQ TYPES (CDR TYPES)))
                                     'A2S))
                                  (CAR ARG))
                                 NIL)))
               LOOPLABEL
                (SETQ ARG (CDR ARG))
                (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ARG)
                            (RL_CONVERTARG ARG
                             (PROG1 (CAR TYPES) (SETQ TYPES (CDR TYPES)))
                             'A2S))
                          (CAR ARG))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (APPLY RL_*B RARGS))
      (COND ((RL_EXCP W) (RL_EXCERR W)))
      (SETQ W (RL_CONVERTARG W RTYPE 'S2A))
      (RETURN W))) 
(PUT 'RL_CONVERTARG 'NUMBER-OF-ARGS 3) 
(DE RL_CONVERTARG (X TYPE X2Y)
    (APPLY (RL_CONVERSIONFUNCTION TYPE X2Y) (LIST X))) 
(PUT 'RL_CONVERSIONFUNCTION 'NUMBER-OF-ARGS 2) 
(DE RL_CONVERSIONFUNCTION (TYPE X2Y)
    (PROG (SUPER F FL KWL)
      (COND
       ((IDP TYPE)
        (PROGN
         (SETQ F (RL_TYPEENTRY TYPE X2Y))
         (COND (F (RETURN F)))
         (SETQ SUPER (RL_TYPEINHERITS TYPE))
         (COND (SUPER (RETURN (RL_CONVERSIONFUNCTION SUPER X2Y))))
         (REDERR (LIST "missing" X2Y "conversion for type" TYPE)))))
      (COND
       ((AND (EQ (INTERN (CAR TYPE)) 'ENUM) (EQ X2Y 'A2S))
        (PROGN
         (SETQ KWL
                 (PROG (H FORALL-RESULT FORALL-ENDPTR)
                   (SETQ H (CDR TYPE))
                   (COND ((NULL H) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (H) (INTERN H)) (CAR H))
                                         NIL)))
                  LOOPLABEL
                   (SETQ H (CDR H))
                   (COND ((NULL H) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (H) (INTERN H)) (CAR H)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (RETURN
          (LIST 'LAMBDA '(X)
                (LIST 'APPLY (LIST 'FUNCTION 'RL_A2SKEYWORD)
                      (LIST 'LIST 'X (MKQUOTE KWL)))))
         NIL)))
      (COND
       ((AND (EQ (INTERN (CAR TYPE)) 'ENUM) (EQ X2Y 'S2A))
        (RETURN (LIST 'LAMBDA '(X) 'X))))
      (PROG (G135)
        (SETQ G135
                (PROG (TY FORALL-RESULT FORALL-ENDPTR)
                  (SETQ TY TYPE)
                  (COND ((NULL TY) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (TY)
                                      (LIST 'FUNCTION
                                            (RL_CONVERSIONFUNCTION TY X2Y)))
                                    (CAR TY))
                                   NIL)))
                 LOOPLABEL
                  (SETQ TY (CDR TY))
                  (COND ((NULL TY) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (TY)
                              (LIST 'FUNCTION (RL_CONVERSIONFUNCTION TY X2Y)))
                            (CAR TY))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
        (SETQ F (CAR G135))
        (SETQ FL (CDR G135))
        (RETURN G135))
      (RETURN
       (LIST 'LAMBDA '(X) (LIST 'APPLY F (LIST 'CONS 'X (CONS 'LIST FL))))))) 
(PUT 'RL_A2SKEYWORD 'NUMBER-OF-ARGS 2) 
(DE RL_A2SKEYWORD (X KEYWORDS)
    (COND ((MEMQ X KEYWORDS) X)
          (T
           ((LAMBDA (*LOWER *RAISE)
              (TYPERR X (IOTO_SMAPRIN (CONS '|oNEoF| KEYWORDS))))
            NIL NIL)))) 
(PUT 'ENUM 'RL_SUPPORT 'RL_TYPE) 
(PUT 'ENUM 'RL_TYPE
     '((DOC (DESCRIPTION . "literal enumeration of admissible Redlog keywords")
        (EXAMPLE . "Enum(auto, cnf, dnf)")))) 
(PUT 'RL_SMSERVICEP 'NUMBER-OF-ARGS 1) 
(DE RL_SMSERVICEP (X) (AND (IDP X) (EQ (GET X 'RL_SUPPORT) 'RL_SMSERVICE))) 
(PUT 'RL_AMSERVICEP 'NUMBER-OF-ARGS 1) 
(DE RL_AMSERVICEP (X) (AND (IDP X) (EQ (GET X 'RL_SUPPORT) 'RL_AMSERVICE))) 
(PUT 'RL_KNOWNIMPLEMENTATIONS 'NUMBER-OF-ARGS 1) 
(DE RL_KNOWNIMPLEMENTATIONS (X) (GET X 'RL_KNOWNIMPLEMENTATIONS)) 
(PUT 'RL_EXC 'NUMBER-OF-ARGS 1) 
(DE RL_EXC (X) (CONS '*RL_EXC* X)) 
(FLAG '(*RL_EXC*) 'ASSERT_IGNORE) 
(PUT 'RL_EXCP 'NUMBER-OF-ARGS 1) 
(DE RL_EXCP (X) (EQCAR X '*RL_EXC*)) 
(PUT 'RL_EXCERR 'NUMBER-OF-ARGS 1) 
(DE RL_EXCERR (EXC) (REDERR (CDR EXC))) 
(PUT 'RL_EXCEPTION 'NUMBER-OF-ARGS 1) 
(DE RL_EXCEPTION (X) (RL_EXC X)) 
(PUT 'RL_EXCEPTIONP 'NUMBER-OF-ARGS 1) 
(DE RL_EXCEPTIONP (X) (RL_EXCP X)) 
(ENDMODULE) 