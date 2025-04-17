(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSUSER)) 
(FLUID '(ALGLIST* SUBFG* *ARBVARS XVARS*)) 
(GLOBAL '(INDXL*)) 
(PUT 'INDEX_EXPAND 'RTYPEFN 'QUOTELIST) 
(PUT 'INDEX_EXPAND 'LISTFN 'INDEXEXPANDEVAL0) 
(PUT 'INDEXEXPANDEVAL0 'NUMBER-OF-ARGS 2) 
(PUT 'INDEXEXPANDEVAL0 'DEFINED-ON-LINE '41) 
(PUT 'INDEXEXPANDEVAL0 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'INDEXEXPANDEVAL0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE INDEXEXPANDEVAL0 (U V)
    (CONS 'LIST
          (PROG (P FORALL-RESULT FORALL-ENDPTR)
            (SETQ P (GETRLIST (INDEXEXPANDEVAL U)))
            (COND ((NULL P) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (P) (REVAL1 P V)) (CAR P)) NIL)))
           LOOPLABEL
            (SETQ P (CDR P))
            (COND ((NULL P) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (P) (REVAL1 P V)) (CAR P)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'INDEXEXPANDEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXEXPANDEVAL 'DEFINED-ON-LINE '47) 
(PUT 'INDEXEXPANDEVAL 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'INDEXEXPANDEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXEXPANDEVAL (U)
    (COND
     ((NEQ (LENGTH U) 1)
      (RERROR 'EDS 0 "Wrong number of arguments to index_expand"))
     ((EQCAR (SETQ U (REVAL1 (CAR U) T)) 'LIST)
      (CONS 'LIST
            (PURGE
             (PROG (X FORALL-RESULT FORALL-ENDPTR)
               (SETQ X (CDR U))
              STARTOVER
               (COND ((NULL X) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       ((LAMBDA (X) (CDR (INDEXEXPANDEVAL (LIST X)))) (CAR X)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
               (SETQ X (CDR X))
               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
              LOOPLABEL
               (COND ((NULL X) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       ((LAMBDA (X) (CDR (INDEXEXPANDEVAL (LIST X)))) (CAR X)))
               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
               (SETQ X (CDR X))
               (GO LOOPLABEL)))))
     (T (CONS 'LIST (INDEXEXPAND U))))) 
(PUT 'INDEXEXPAND 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXEXPAND 'DEFINED-ON-LINE '56) 
(PUT 'INDEXEXPAND 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'INDEXEXPAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXEXPAND (U)
    (COND ((EQEXPR U) (INDEXEXPANDEQN U)) ((BOOLEXPR U) (INDEXEXPANDBOOL U))
          (T
           (PROG (I V ALGLIST*)
             (SETQ ALGLIST* (CONS NIL NIL))
             (SETQ U (SIMP* U))
             (SETQ I
                     (IDSORT
                      (PURGE
                       (COND
                        ((AND (CAR U)
                              (NOT (OR (ATOM (CAR U)) (ATOM (CAR (CAR U))))))
                         (FLATINDXL (ALLIND (LIST (CAR (CAR U))))))))))
             (SETQ V
                     (PROG (J FORALL-RESULT FORALL-ENDPTR)
                       (SETQ J (MKAINDXC I NIL))
                      STARTOVER
                       (COND ((NULL J) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               ((LAMBDA (J)
                                  (COND
                                   ((CAR
                                     (SETQ J
                                             (SUBFREEINDICES (CAR U)
                                              (PAIR I J))))
                                    (LIST (CONS (ABSF (CAR J)) (CDR J))))))
                                (CAR J)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                       (SETQ J (CDR J))
                       (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                      LOOPLABEL
                       (COND ((NULL J) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               ((LAMBDA (J)
                                  (COND
                                   ((CAR
                                     (SETQ J
                                             (SUBFREEINDICES (CAR U)
                                              (PAIR I J))))
                                    (LIST (CONS (ABSF (CAR J)) (CDR J))))))
                                (CAR J)))
                       (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                       (SETQ J (CDR J))
                       (GO LOOPLABEL)))
             (RETURN
              (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                (SETQ Q (PURGE V))
                (COND ((NULL Q) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Q)
                                    (MK*SQ (MULTSQ Q (CONS 1 (CDR U)))))
                                  (CAR Q))
                                 NIL)))
               LOOPLABEL
                (SETQ Q (CDR Q))
                (COND ((NULL Q) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (Q) (MK*SQ (MULTSQ Q (CONS 1 (CDR U)))))
                          (CAR Q))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'INDEXEXPANDEQN 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXEXPANDEQN 'DEFINED-ON-LINE '74) 
(PUT 'INDEXEXPANDEQN 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'INDEXEXPANDEQN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXEXPANDEQN (U)
    (PROG (I V LH RH ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      ((LAMBDA (SUBFG*) (SETQ LH (REVAL1 (CADR U) T))) NIL)
      (SETQ I (IDSORT (PURGE (FLATINDXL (ALLINDK LH)))))
      (SETQ RH (REVAL1 (CADDR U) NIL))
      (SETQ V
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (MKAINDXC I NIL))
               STARTOVER
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (J)
                           (COND
                            ((SETQ J
                                     (SUBFREEINDEQN (LIST (CAR U) LH RH)
                                      (PAIR I J)))
                             (LIST J))))
                         (CAR J)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ J (CDR J))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (J)
                           (COND
                            ((SETQ J
                                     (SUBFREEINDEQN (LIST (CAR U) LH RH)
                                      (PAIR I J)))
                             (LIST J))))
                         (CAR J)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ J (CDR J))
                (GO LOOPLABEL)))
      (SETQ I (LIST))
      (SETQ V
              (PROG (R FORALL-RESULT FORALL-ENDPTR)
                (SETQ R V)
               STARTOVER
                (COND ((NULL R) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (R)
                           (COND
                            ((NOT (MEMBER (CADR R) I))
                             (PROGN (SETQ I (CONS (CADR R) I)) (LIST R)))))
                         (CAR R)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ R (CDR R))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL R) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (R)
                           (COND
                            ((NOT (MEMBER (CADR R) I))
                             (PROGN (SETQ I (CONS (CADR R) I)) (LIST R)))))
                         (CAR R)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ R (CDR R))
                (GO LOOPLABEL)))
      (RETURN V))) 
(PUT 'SUBFREEINDEQN 'NUMBER-OF-ARGS 2) 
(PUT 'SUBFREEINDEQN 'DEFINED-ON-LINE '93) 
(PUT 'SUBFREEINDEQN 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'SUBFREEINDEQN 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBFREEINDEQN (U L)
    (PROG (LH RH)
      (SETQ LH (SUBFREEINDK (CADR U) L))
      (COND ((NULL (ATOMF LH)) (SETQ LH (REVOP1 LH))))
      ((LAMBDA (SUBFG*) (SETQ LH (REVAL1 LH T))) NIL)
      (COND ((EQUAL LH 0) (RETURN NIL)))
      (SETQ RH (SIMP* (CADDR U)))
      (SETQ RH
              (MULTSQ (SUBFREEINDICES (CAR RH) L)
                      (INVSQ (SUBFREEINDICES (CDR RH) L))))
      (COND
       ((EQCAR LH 'MINUS) (PROGN (SETQ LH (CADR LH)) (SETQ RH (NEGSQ RH)))))
      (RETURN (LIST (CAR U) LH (MK*SQ RH))))) 
(PUT 'BOOLEXPR 'NUMBER-OF-ARGS 1) 
(PUT 'BOOLEXPR 'DEFINED-ON-LINE '113) 
(PUT 'BOOLEXPR 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'BOOLEXPR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BOOLEXPR (U) (AND (NOT (ATOM U)) (FLAGP (CAR U) 'BOOLEAN))) 
(PUT 'INDEXEXPANDBOOL 'NUMBER-OF-ARGS 1) 
(PUT 'INDEXEXPANDBOOL 'DEFINED-ON-LINE '118) 
(PUT 'INDEXEXPANDBOOL 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'INDEXEXPANDBOOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE INDEXEXPANDBOOL (U)
    (PROG (I V ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (SETQ I (IDSORT (PURGE (FLATINDXL (ALLINDK U)))))
      (SETQ V
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (MKAINDXC I NIL))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (CONS (CAR U)
                                          (PROG (A FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ A (CDR U))
                                            (COND ((NULL A) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (A)
                                                                (REVAL1
                                                                 (SUBFREEINDK A
                                                                  (PAIR I J))
                                                                 T))
                                                              (CAR A))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ A (CDR A))
                                            (COND
                                             ((NULL A) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (A)
                                                        (REVAL1
                                                         (SUBFREEINDK A
                                                          (PAIR I J))
                                                         T))
                                                      (CAR A))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL))))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J)
                            (CONS (CAR U)
                                  (PROG (A FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ A (CDR U))
                                    (COND ((NULL A) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (A)
                                                        (REVAL1
                                                         (SUBFREEINDK A
                                                          (PAIR I J))
                                                         T))
                                                      (CAR A))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ A (CDR A))
                                    (COND ((NULL A) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (A)
                                                (REVAL1
                                                 (SUBFREEINDK A (PAIR I J)) T))
                                              (CAR A))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL))))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (RETURN (PURGE V)))) 
(PUT 'SUBFREEINDICES 'NUMBER-OF-ARGS 2) 
(PUT 'SUBFREEINDICES 'DEFINED-ON-LINE '130) 
(PUT 'SUBFREEINDICES 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'SUBFREEINDICES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBFREEINDICES (U L)
    (PROG (ALGLIST*)
      (SETQ ALGLIST* (CONS NIL NIL))
      (RETURN
       (COND ((OR (ATOM U) (ATOM (CAR U))) (CONS U 1))
             (T
              (ADDSQ
               (MULTSQ
                (COND ((ATOM (CAAAR U)) (CONS (LIST (CONS (CAAR U) 1)) 1))
                      ((SFP (CAAAR U))
                       (EXPTSQ (SUBFREEINDICES (CAAAR U) L) (CDAAR U)))
                      (T (EXPTSQ (SIMP (SUBFREEINDK (CAAAR U) L)) (CDAAR U))))
                (SUBFREEINDICES (CDAR U) L))
               (SUBFREEINDICES (CDR U) L))))))) 
(PUT 'SUBFREEINDK 'NUMBER-OF-ARGS 2) 
(PUT 'SUBFREEINDK 'DEFINED-ON-LINE '150) 
(PUT 'SUBFREEINDK 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'SUBFREEINDK 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SUBFREEINDK (U L)
    (COND ((ATOM U) U)
          ((FLAGP (CAR U) 'INDEXVAR) (CONS (CAR U) (SUBLA L (CDR U))))
          (T (SUBINDK L U)))) 
(PUT 'LINEAR_DIVISORS 'RTYPEFN 'QUOTELIST) 
(PUT 'LINEAR_DIVISORS 'LISTFN 'LINEARDIVISORS) 
(PUT 'LINEARDIVISORS 'NUMBER-OF-ARGS 2) 
(PUT 'LINEARDIVISORS 'DEFINED-ON-LINE '162) 
(PUT 'LINEARDIVISORS 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'LINEARDIVISORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LINEARDIVISORS (U V)
    (CONS 'LIST
          (PROG (F FORALL-RESULT FORALL-ENDPTR)
            (SETQ F (LINEARDIVISORSPF (XPARTITOP (REVAL1 (CAR U) T))))
            (COND ((NULL F) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (F) (*PF2A1 F V)) (CAR F)) NIL)))
           LOOPLABEL
            (SETQ F (CDR F))
            (COND ((NULL F) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (F) (*PF2A1 F V)) (CAR F)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'LINEARDIVISORSPF 'NUMBER-OF-ARGS 1) 
(PUT 'LINEARDIVISORSPF 'DEFINED-ON-LINE '168) 
(PUT 'LINEARDIVISORSPF 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'LINEARDIVISORSPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LINEARDIVISORSPF (F)
    (PROG (X G V)
      (PROG (P)
        (SETQ P (XPOWS F))
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P) (SETQ X (UNION (WEDGEFAX P) X))) (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (PROG (K)
        (SETQ K X)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (PROGN
            (SETQ V (CONS (INTERN (GENSYM)) V))
            (SETQ G
                    (ADDPF
                     (CONS (CONS K (CONS (LIST (CONS (CONS (CAR V) 1) 1)) 1))
                           NIL)
                     G))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (SETQ X (EDSSOLVE (XCOEFFS (WEDGEPF G F)) V))
      (COND
       ((OR (NEQ (LENGTH X) 1) (NEQ (CAAR X) T))
        (ERRDHH "Bad solve result in lineardivisorspf")))
      (SETQ X (CADAR X))
      (SETQ V (UPDKORDL V))
      (SETQ G (CAR (SUBF (CAR (*PF2SQ G)) X)))
      (SETQ X (LIST))
      (PROG ()
       WHILELABEL
        (COND ((NOT G) (RETURN NIL)))
        (PROGN
         (SETQ X (CONS (XPARTITSQ (CONS (CDAR G) 1)) X))
         (SETQ G (CDR G)))
        (GO WHILELABEL))
      (SETKORDER V)
      (RETURN (REVERSE (XAUTOREDUCE X))))) 
(PUT 'XDECOMPOSEPF 'NUMBER-OF-ARGS 1) 
(PUT 'XDECOMPOSEPF 'DEFINED-ON-LINE '190) 
(PUT 'XDECOMPOSEPF 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'XDECOMPOSEPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XDECOMPOSEPF (F)
    (PROG (X)
      (SETQ X (LINEARDIVISORSPF F))
      (COND ((EQUAL (LENGTH X) (DEGREEPF F)) (RETURN (REVERSE X)))))) 
(PUT 'EXFACTORS 'RTYPEFN 'QUOTELIST) 
(PUT 'EXFACTORS 'LISTFN 'EXFACTORS) 
(PUT 'EXFACTORS 'NUMBER-OF-ARGS 2) 
(PUT 'EXFACTORS 'DEFINED-ON-LINE '201) 
(PUT 'EXFACTORS 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'EXFACTORS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EXFACTORS (U V)
    (CONS 'LIST
          (PROG (F FORALL-RESULT FORALL-ENDPTR)
            (SETQ F (XFACTORSPF (XPARTITOP (REVAL1 (CAR U) T))))
            (COND ((NULL F) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS ((LAMBDA (F) (*PF2A1 F V)) (CAR F)) NIL)))
           LOOPLABEL
            (SETQ F (CDR F))
            (COND ((NULL F) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (F) (*PF2A1 F V)) (CAR F)) NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'XFACTORSPF 'NUMBER-OF-ARGS 1) 
(PUT 'XFACTORSPF 'DEFINED-ON-LINE '207) 
(PUT 'XFACTORSPF 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'XFACTORSPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE XFACTORSPF (F)
    (PROG (X)
      (SETQ X (LINEARDIVISORSPF F))
      (SETQ F
              (XREDUCE F
                       (PROG (G FORALL-RESULT FORALL-ENDPTR)
                         (SETQ G X)
                         (COND ((NULL G) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (G)
                                             (ADDPF
                                              (CONS (CONS 1 (CONS (MINUS 1) 1))
                                                    NIL)
                                              G))
                                           (CAR G))
                                          NIL)))
                        LOOPLABEL
                         (SETQ G (CDR G))
                         (COND ((NULL G) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (G)
                                     (ADDPF
                                      (CONS (CONS 1 (CONS (MINUS 1) 1)) NIL)
                                      G))
                                   (CAR G))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
      (RETURN
       (COND ((EQUAL F (CONS (CONS 1 (CONS 1 1)) NIL)) (REVERSE X))
             (T (CONS F (REVERSE X))))))) 
(FLAG '(EXACT) 'OPFN) 
(PUT 'EXACT 'NUMBER-OF-ARGS 1) 
(PUT 'EXACT 'DEFINED-ON-LINE '220) 
(PUT 'EXACT 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'EXACT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EXACT (U) (EQCAR U 'D)) 
(FLAG '(EXACT) 'BOOLEAN) 
(PUT 'DERIVED_SYSTEM 'RTYPEFN 'GETRTYPECAR) 
(PUT 'DERIVED_SYSTEM 'EDSFN 'DERIVEEDS) 
(PUT 'DERIVED_SYSTEM 'LISTFN 'DERIVELIST) 
(PUT 'DERIVELIST 'NUMBER-OF-ARGS 2) 
(PUT 'DERIVELIST 'DEFINED-ON-LINE '232) 
(PUT 'DERIVELIST 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'DERIVELIST 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE DERIVELIST (U V)
    ((LAMBDA (XVARS*) (*SYS2A1 (DERIVE (*A2SYS (REVAL1 (CAR U) T))) V)) NIL)) 
(PUT 'DERIVEEDS 'NUMBER-OF-ARGS 1) 
(PUT 'DERIVEEDS 'DEFINED-ON-LINE '237) 
(PUT 'DERIVEEDS 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'DERIVEEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DERIVEEDS (S)
    (PROG ()
      (SETQ S (COPYEDS S))
      (COND ((PFAFFIAN S) (SETCAR (CDR S) (DERIVE (PFAFFPART (CADR S)))))
            (T (RERROR 'EDS 0 "non-Pfaffian system in derived_system")))
      (RETURN S))) 
(PUT 'DERIVE 'NUMBER-OF-ARGS 1) 
(PUT 'DERIVE 'DEFINED-ON-LINE '248) 
(PUT 'DERIVE 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'DERIVE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE DERIVE (S)
    (PROG (C F)
      (COND ((NULL S) S))
      (SETQ S (XAUTOREDUCE S))
      (SETQ C
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F S)
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (F)
                                    (COND
                                     ((EQUAL (DEGREEPF F) 1) (INTERN (GENSYM)))
                                     (T
                                      (RERROR 'EDS 0
                                              "non-Pfaffian system in derived_system"))))
                                  (CAR F))
                                 NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (F)
                            (COND ((EQUAL (DEGREEPF F) 1) (INTERN (GENSYM)))
                                  (T
                                   (RERROR 'EDS 0
                                           "non-Pfaffian system in derived_system"))))
                          (CAR F))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ F
              (ZIPPF S
               (PROG (K FORALL-RESULT FORALL-ENDPTR)
                 (SETQ K C)
                 (COND ((NULL K) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (K)
                                     (CONS (LIST (CONS (CONS K 1) 1)) 1))
                                   (CAR K))
                                  NIL)))
                LOOPLABEL
                 (SETQ K (CDR K))
                 (COND ((NULL K) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS
                          ((LAMBDA (K) (CONS (LIST (CONS (CONS K 1) 1)) 1))
                           (CAR K))
                          NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (SETQ S (EDSSOLVE (XCOEFFS (XREDUCE (EXDFPF F) S)) C))
      (COND
       ((OR (NEQ (LENGTH S) 1) (NULL (CAAR S)))
        (ERRDHH (LIST "Bad solve result in derive:" S))))
      (SETQ S (CADR (CAR S)))
      (SETQ F (PULLBACKPF F S))
      (SETQ C
              (SETDIFF C
                       (PROG (M FORALL-RESULT FORALL-ENDPTR)
                         (SETQ M S)
                         (COND ((NULL M) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS ((LAMBDA (M) (CAR M)) (CAR M))
                                               NIL)))
                        LOOPLABEL
                         (SETQ M (CDR M))
                         (COND ((NULL M) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS ((LAMBDA (M) (CAR M)) (CAR M)) NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
      ((LAMBDA (XVARS*) (SETQ F (XREPARTIT F))) C)
      (RETURN
       (PROG (X FORALL-RESULT FORALL-ENDPTR)
         (SETQ X (REVERSE C))
         (COND ((NULL X) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (X) (XCOEFF F (LIST X))) (CAR X))
                               NIL)))
        LOOPLABEL
         (SETQ X (CDR X))
         (COND ((NULL X) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (X) (XCOEFF F (LIST X))) (CAR X)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))) 
(PUT 'ALLCOORDS 'NUMBER-OF-ARGS 1) 
(PUT 'ALLCOORDS 'DEFINED-ON-LINE '268) 
(PUT 'ALLCOORDS 'DEFINED-IN-FILE 'EDS/EDSUSER.RED) 
(PUT 'ALLCOORDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE ALLCOORDS (F)
    (CONS 'LIST
          (PURGE
           (PROG (K FORALL-RESULT FORALL-ENDPTR)
             (SETQ K ((LAMBDA (XVARS*) (XVARSPF (XPARTITOP F))) T))
            STARTOVER
             (COND ((NULL K) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (K)
                        (COND
                         ((AND (EQUAL (XDEGREE K) 0) (NOT (ASSOC K DEPL*))
                               (NOT (EQCAR K 'PARTDF)))
                          (LIST K))
                         ((AND (EQUAL (XDEGREE K) 1) (EXACT K))
                          (LIST (CADR K)))))
                      (CAR K)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ K (CDR K))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL K) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (K)
                        (COND
                         ((AND (EQUAL (XDEGREE K) 0) (NOT (ASSOC K DEPL*))
                               (NOT (EQCAR K 'PARTDF)))
                          (LIST K))
                         ((AND (EQUAL (XDEGREE K) 1) (EXACT K))
                          (LIST (CADR K)))))
                      (CAR K)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ K (CDR K))
             (GO LOOPLABEL))))) 
(ENDMODULE) 