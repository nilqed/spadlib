(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'EDSPDE)) 
(FLUID '(XVARS* KORD* DEPL* DEPENDENCIES)) 
(GLOBAL '(INDXL*)) 
(PUT 'PDE2EDS 'RTYPEFN 'QUOTEEDS) 
(PUT 'PDE2EDS 'EDSFN 'PDE2EDS) 
(FLAG '(PDE2EDS) 'NOSPREAD) 
(PUT 'PDE2EDS 'NUMBER-OF-ARGS 1) 
(PUT 'PDE2EDS 'DEFINED-ON-LINE '40) 
(PUT 'PDE2EDS 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT 'PDE2EDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PDE2EDS (U)
    (PROG (PDE DEP IND VARS FNS S MAP)
      (SETQ PDE (PDE2JET U))
      (SETQ DEP (CADR PDE))
      (SETQ IND (CADDR PDE))
      (SETQ FNS (CADDDR PDE))
      (SETQ PDE (GETRLIST (CAR PDE)))
      (SETQ VARS
              (APPEND IND
                      (PROG (P FORALL-RESULT FORALL-ENDPTR)
                        (SETQ P DEP)
                        (COND ((NULL P) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                (SETQ FORALL-ENDPTR
                                        (CONS ((LAMBDA (P) (CAR P)) (CAR P))
                                              NIL)))
                       LOOPLABEL
                        (SETQ P (CDR P))
                        (COND ((NULL P) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                        (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                        (GO LOOPLABEL))))
      (SETQ DEPENDENCIES
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST
                     (PURGE
                      (PROG (K FORALL-RESULT FORALL-ENDPTR)
                        (SETQ K (APPEND FNS VARS))
                       STARTOVER
                        (COND ((NULL K) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (K)
                                   (COND
                                    ((SETQ K (ATSOC (LID K) DEPL*))
                                     (LIST (CONS 'LIST K)))))
                                 (CAR K)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ K (CDR K))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL K) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (K)
                                   (COND
                                    ((SETQ K (ATSOC (LID K) DEPL*))
                                     (LIST (CONS 'LIST K)))))
                                 (CAR K)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ K (CDR K))
                        (GO LOOPLABEL))))))
      (PROG (K)
        (SETQ K VARS)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (PROGN
            (SETQ K (LID K))
            (PROG (X)
              (SETQ X (ATSOC K DEPL*))
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (DEPEND1 K X NIL)) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (REMFLAG (LIST K) 'IMPFUN)
            NIL))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (PROG (K)
        (SETQ K FNS)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (PROGN
            (SETQ K (LID K))
            (PROG (X)
              (SETQ X (SETDIFF (ATSOC K DEPL*) IND))
             LAB
              (COND ((NULL X) (RETURN NIL)))
              ((LAMBDA (X) (DEPEND1 K X NIL)) (CAR X))
              (SETQ X (CDR X))
              (GO LAB))
            (COND
             ((NULL (ATSOC K DEPL*))
              (PROG (X)
                (SETQ X IND)
               LAB
                (COND ((NULL X) (RETURN NIL)))
                ((LAMBDA (X) (DEPEND1 K X T)) (CAR X))
                (SETQ X (CDR X))
                (GO LAB))))
            (FLAG (LIST K) 'IMPFUN)
            NIL))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (SETQ VARS (LIST))
      (PROG (P)
        (SETQ P DEP)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (COND
            ((SETQ S (ASSOC (CDR P) VARS)) (SETCDR S (CONS (CAR P) (CDR S))))
            (T (SETQ VARS (CONS (CONS (CDR P) (LIST (CAR P))) VARS)))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (SETQ S
              (PARTIALCONTACT
               (CONS 'LIST
                     (PROG (L FORALL-RESULT FORALL-ENDPTR)
                       (SETQ L VARS)
                       (COND ((NULL L) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (L) (CONS 'LIST L)) (CAR L))
                                        NIL)))
                      LOOPLABEL
                       (SETQ L (CDR L))
                       (COND ((NULL L) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (L) (CONS 'LIST L)) (CAR L))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
               (CONS 'LIST IND)))
      (SETQ MAP
              (CONS 'LIST
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X PDE)
                     STARTOVER
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (X)
                                 (COND
                                  ((AND (EQEXPR X) (KERNP (SIMP* (CADR X))))
                                   (LIST X))))
                               (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ X (CDR X))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (X)
                                 (COND
                                  ((AND (EQEXPR X) (KERNP (SIMP* (CADR X))))
                                   (LIST X))))
                               (CAR X)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ X (CDR X))
                      (GO LOOPLABEL))))
      (SETQ PDE
              (CONS 'LIST
                    (PROG (X FORALL-RESULT FORALL-ENDPTR)
                      (SETQ X (SETDIFF PDE MAP))
                      (COND ((NULL X) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS ((LAMBDA (X) (*EQN2A X)) (CAR X))
                                            NIL)))
                     LOOPLABEL
                      (SETQ X (CDR X))
                      (COND ((NULL X) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS ((LAMBDA (X) (*EQN2A X)) (CAR X)) NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL))))
      (COND (PDE (SETQ S (EDSPROTECT (LIST 'AUGMENTEDS S PDE)))))
      (COND (MAP (SETQ S (EDSPROTECT (LIST 'PULLBACKEDS S MAP)))))
      (RETURN S))) 
(PUT 'PARTIAL_CONTACT 'RTYPEFN 'QUOTEEDS) 
(PUT 'PARTIAL_CONTACT 'EDSFN 'PARTIALCONTACT) 
(PUT 'PARTIALCONTACT 'NUMBER-OF-ARGS 2) 
(PUT 'PARTIALCONTACT 'DEFINED-ON-LINE '86) 
(PUT 'PARTIALCONTACT 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT 'PARTIALCONTACT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PARTIALCONTACT (VARS IND)
    (PROG (S JET ORD SYS)
      (SETQ VARS
              (PROG (L FORALL-RESULT FORALL-ENDPTR)
                (SETQ L (GETRLIST VARS))
                (COND ((NULL L) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (L) (GETRLIST L)) (CAR L))
                                      NIL)))
               LOOPLABEL
                (SETQ L (CDR L))
                (COND ((NULL L) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (L) (GETRLIST L)) (CAR L)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS (SORT VARS (FUNCTION GREATERPCAR)))
      (SETQ IND (*A2CFRM (LIST (CONS 'LIST (GETRLIST IND)))))
      (SETQ S
              (MKEDS
               (LIST (LIST)
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F (CADR IND))
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (F)
                                           (CONS (CONS F (CONS 1 1)) NIL))
                                         (CAR F))
                                        NIL)))
                      LOOPLABEL
                       (SETQ F (CDR F))
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (F) (CONS (CONS F (CONS 1 1)) NIL))
                                 (CAR F))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     IND NIL)))
      (PUTEDS S 'SQVAR *SQVAR*)
      (PROG (F)
        (SETQ F (LIST 'SOLVED 'REDUCED 'QUASILINEAR 'PFAFFIAN 'INVOLUTIVE))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (FLAGTRUEEDS S F)) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (PROG ()
       WHILELABEL
        (COND ((NOT VARS) (RETURN NIL)))
        (PROGN
         (SETQ JET (*A2CFRM (LIST (CONS 'LIST (CDAR VARS)))))
         (SETCAR (CDDR (CDR S)) (CFRMPROD2 (CADDR (CDR S)) JET))
         (PUTEDS S 'JET0 (APPEND (GETEDS S 'JET0) (CDAR VARS)))
         (SETQ ORD
                 (COND ((CDR VARS) (DIFFERENCE (CAAR VARS) (CAADR VARS)))
                       (T (CAAR VARS))))
         (PROG (I)
           (SETQ I 1)
          LAB
           (COND ((MINUSP (DIFFERENCE ORD I)) (RETURN NIL)))
           (PROGN
            (SETQ SYS (CADR S))
            (SETQ S (EDSPROTECT (LIST 'GBSYS S)))
            (SETCAR (CDR S) (APPEND SYS (CADR S))))
           (SETQ I (PLUS2 I 1))
           (GO LAB))
         (SETQ VARS (CDR VARS)))
        (GO WHILELABEL))
      (RETURN S))) 
(PUT 'PDE2JET 'RTYPEFN 'QUOTELIST) 
(PUT 'PDE2JET 'LISTFN 'PDE2JETEVAL) 
(PUT 'PDE2JETEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'PDE2JETEVAL 'DEFINED-ON-LINE '114) 
(PUT 'PDE2JETEVAL 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT 'PDE2JETEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE PDE2JETEVAL (U V) (REVAL1 (CAR (PDE2JET (REVLIS U))) V)) 
(PUT 'PDE2JET 'NUMBER-OF-ARGS 1) 
(PUT 'PDE2JET 'DEFINED-ON-LINE '118) 
(PUT 'PDE2JET 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT 'PDE2JET 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PDE2JET (U)
    (PROG (DEP1 IND1 DRV IND DEP FNS IDXS RLB *EVALLHSEQP)
      (COND
       ((AND (NEQ (LENGTH U) 1) (NEQ (LENGTH U) 3))
        (RERROR 'EDS 0 "Wrong number of arguments to pde2jet")))
      (COND
       ((GREATERP (LENGTH U) 1)
        (PROGN
         (SETQ DEP1
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V (GETRLIST (CADR U)))
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (V) (*A2K V)) (CAR V)) NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (V) (*A2K V)) (CAR V)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL)))
         (SETQ IND1
                 (PROG (V FORALL-RESULT FORALL-ENDPTR)
                   (SETQ V (GETRLIST (CADDR U)))
                   (COND ((NULL V) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS ((LAMBDA (V) (*A2K V)) (CAR V)) NIL)))
                  LOOPLABEL
                   (SETQ V (CDR V))
                   (COND ((NULL V) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (V) (*A2K V)) (CAR V)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))))
      (ON (LIST 'EVALLHSEQP))
      (PROG (X)
        (SETQ X (GETRLIST (CAR U)))
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X) (SETQ DRV (UNION (EDSDFKERNELS X) DRV))) (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (EDSDEBUG "Derivatives and functions found" DRV 'COB)
      (SETQ IND (EDSPDESCAN DRV))
      (SETQ DEP (CAR IND))
      (SETQ IND (CADR IND))
      (COND
       ((GREATERP (LENGTH U) 1)
        (PROGN
         (COND
          ((NOT (SUBSETP IND IND1))
           (RERROR 'EDS 0
                   "Less independent variables given than occur in PDE")))
         (SETQ IND IND1)
         (PROG (K)
           (SETQ K IND)
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K) (SETQ DEP (DELASC K DEP))) (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         (SETQ FNS
                 (SETDIFF
                  (PROG (P FORALL-RESULT FORALL-ENDPTR)
                    (SETQ P DEP)
                    (COND ((NULL P) (RETURN NIL)))
                    (SETQ FORALL-RESULT
                            (SETQ FORALL-ENDPTR
                                    (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL)))
                   LOOPLABEL
                    (SETQ P (CDR P))
                    (COND ((NULL P) (RETURN FORALL-RESULT)))
                    (RPLACD FORALL-ENDPTR
                            (CONS ((LAMBDA (P) (CAR P)) (CAR P)) NIL))
                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                    (GO LOOPLABEL))
                  DEP1))
         (PROG (K)
           (SETQ K FNS)
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K) (SETQ DEP (DELASC K DEP))) (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         (PROG (K)
           (SETQ K DEP1)
          LAB
           (COND ((NULL K) (RETURN NIL)))
           ((LAMBDA (K)
              (COND ((NOT (ATSOC K DEP)) (SETQ DEP (CONS (CONS K 0) DEP)))))
            (CAR K))
           (SETQ K (CDR K))
           (GO LAB))
         NIL)))
      (SETQ DEP (SORT DEP (FUNCTION ORDOPCAR)))
      (SETQ IND (SORT IND (FUNCTION ORDOP)))
      (SETQ FNS (SORT FNS (FUNCTION ORDOP)))
      (EDSDEBUG "Dependent variables and orders"
       (CONS 'LIST
             (PROG (P FORALL-RESULT FORALL-ENDPTR)
               (SETQ P DEP)
               (COND ((NULL P) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P)))
                                 (CAR P))
                                NIL)))
              LOOPLABEL
               (SETQ P (CDR P))
               (COND ((NULL P) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (P) (LIST 'EQUAL (CAR P) (CDR P))) (CAR P))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
       'PREFIX)
      (EDSDEBUG "Independent variables" IND 'COB)
      (EDSDEBUG "Other functions" FNS 'COB)
      (PROG (K)
        (SETQ K (APPEND FNS IND))
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K) (COND ((NOT (EXFORMP K)) (MKFORM* K 0)))) (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (PROG (K)
        (SETQ K DEP)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K) (COND ((NOT (EXFORMP (CAR K))) (MKFORM* (CAR K) 0))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (SETQ IDXS (UNIQIDS IND))
      (COND
       ((NOT (SUBSETP IDXS INDXL*))
        (APPLY1 'INDEXRANGE (LIST (LIST 'EQUAL (GENSYM) (CONS 'LIST IDXS))))))
      (SETQ IDXS (PAIR IND IDXS))
      (PROG (K)
        (SETQ K DRV)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K)
           (COND
            ((OR (EQCAR K 'DF) (EQCAR K 'PARTDF))
             (COND
              ((MEMQ (CADR K) FNS)
               (SETQ RLB (CONS (LIST 'EQUAL K (*DF2PARTDF K)) RLB)))
              (T (SETQ RLB (CONS (LIST 'EQUAL K (*DF2JET K IDXS)) RLB)))))))
         (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (EDSDEBUG "Relabelling list" (CONS 'LIST RLB) 'PREFIX)
      (RETURN (LIST (SUBEVAL (LIST (CONS 'LIST RLB) (CAR U))) DEP IND FNS)))) 
(PUT 'EDSPDESCAN 'NUMBER-OF-ARGS 1) 
(PUT 'EDSPDESCAN 'DEFINED-ON-LINE '182) 
(PUT 'EDSPDESCAN 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT 'EDSPDESCAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSPDESCAN (U)
    (PROG (DEP IND K P)
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ K (CAR U))
         (SETQ U (CDR U))
         (COND
          ((OR (EQCAR K 'PARTDF) (EQCAR K 'DF))
           (PROGN
            (SETQ K (CONS (CADR K) (EDSDFEXPAND (CDDR K))))
            (SETQ IND (UNION (CDR K) IND))
            (COND
             ((SETQ P (ATSOC (CAR K) DEP))
              (SETCDR P (MAX (CDR P) (LENGTH (CDR K)))))
             (T (SETQ DEP (CONS (CONS (CAR K) (LENGTH (CDR K))) DEP))))))
          ((NOT (ATSOC K DEP))
           (COND
            ((OR (ATOM K) ((LAMBDA (XVARS*) (XVARP K)) T))
             (SETQ DEP (CONS (CONS K 0) DEP)))
            (T
             (PROG (V)
               (SETQ V (CDR K))
              LAB
               (COND ((NULL V) (RETURN NIL)))
               ((LAMBDA (V) (SETQ U (UNION (EDSDFKERNELS V) U))) (CAR V))
               (SETQ V (CDR V))
               (GO LAB)))))))
        (GO WHILELABEL))
      (PROG (K)
        (SETQ K IND)
       LAB
        (COND ((NULL K) (RETURN NIL)))
        ((LAMBDA (K) (SETQ DEP (DELASC K DEP))) (CAR K))
        (SETQ K (CDR K))
        (GO LAB))
      (RETURN (LIST DEP IND)))) 
(PUT 'EDSDFKERNELS 'NUMBER-OF-ARGS 1) 
(PUT 'EDSDFKERNELS 'DEFINED-ON-LINE '208) 
(PUT 'EDSDFKERNELS 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT 'EDSDFKERNELS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSDFKERNELS (X)
    (COND ((EQEXPR X) (UNION (EDSDFKERNELS (CADR X)) (EDSDFKERNELS (CADDR X))))
          (T
           (PROGN
            (SETQ X (SIMP* X))
            (PROG (K FORALL-RESULT FORALL-ENDPTR)
              (SETQ K (UNION (KERNELS (CAR X)) (KERNELS (CDR X))))
             STARTOVER
              (COND ((NULL K) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      ((LAMBDA (K)
                         (COND
                          ((OR (EQCAR K 'DF) (EQCAR K 'PARTDF) (ASSOC K DEPL*)
                               (EXFORMP K))
                           (LIST K))))
                       (CAR K)))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
              (SETQ K (CDR K))
              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
             LOOPLABEL
              (COND ((NULL K) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      ((LAMBDA (K)
                         (COND
                          ((OR (EQCAR K 'DF) (EQCAR K 'PARTDF) (ASSOC K DEPL*)
                               (EXFORMP K))
                           (LIST K))))
                       (CAR K)))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
              (SETQ K (CDR K))
              (GO LOOPLABEL)))))) 
(PUT '*DF2JET 'NUMBER-OF-ARGS 2) 
(PUT '*DF2JET 'DEFINED-ON-LINE '220) 
(PUT '*DF2JET 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT '*DF2JET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE *DF2JET (U IDXS)
    (PROG (V IXL)
      (SETQ U (CDR U))
      (COND ((ATOM (SETQ V (CAR U))) (SETQ V (LIST V))))
      (SETQ IXL (SUBLIS IDXS (EDSDFEXPAND (CDR U))))
      (SETQ IXL
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (SORT IXL 'INDTORDP))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (LIST 'MINUS J)) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ U (CAR (FKERN (APPEND V IXL))))
      (RETURN (MKFORM* U 0)))) 
(PUT '*DF2PARTDF 'NUMBER-OF-ARGS 1) 
(PUT '*DF2PARTDF 'DEFINED-ON-LINE '232) 
(PUT '*DF2PARTDF 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT '*DF2PARTDF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE *DF2PARTDF (U)
    (CAR (FKERN (CONS 'PARTDF (CONS (CADR U) (EDSDFEXPAND (CDDR U))))))) 
(PUT 'EDSDFEXPAND 'NUMBER-OF-ARGS 1) 
(PUT 'EDSDFEXPAND 'DEFINED-ON-LINE '237) 
(PUT 'EDSDFEXPAND 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT 'EDSDFEXPAND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE EDSDFEXPAND (U)
    (COND ((NULL U) NIL)
          ((AND (CDR U) (FIXP (CADR U)))
           (NCONC (NLIST (CAR U) (CADR U)) (EDSDFEXPAND (CDDR U))))
          (T (CONS (CAR U) (EDSDFEXPAND (CDR U)))))) 
(FLAG '(MKDEPEND) 'OPFN) 
(PUT 'MKDEPEND 'NUMBER-OF-ARGS 1) 
(PUT 'MKDEPEND 'DEFINED-ON-LINE '249) 
(PUT 'MKDEPEND 'DEFINED-IN-FILE 'EDS/EDSPDE.RED) 
(PUT 'MKDEPEND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE MKDEPEND (U)
    (PROG (V)
      (SETQ V (GETRLIST U))
     LAB
      (COND ((NULL V) (RETURN NIL)))
      ((LAMBDA (V)
         (COND
          ((SETQ V (GETRLIST V))
           (PROGN
            (SETQ DEPL* (CONS V (DELASC (CAR V) DEPL*)))
            (COND
             ((OR (EXFORMP (CAR V)) (FLAGP (CAR V) 'INDEXVAR))
              (FLAG (LIST (CAR V)) 'IMPFUN)))))))
       (CAR V))
      (SETQ V (CDR V))
      (GO LAB))) 
(ENDMODULE) 