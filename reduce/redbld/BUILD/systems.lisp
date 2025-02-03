(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'SYSTEMS)) 
(FLUID '(KORD* XTRUNCATE* *ARBVARS *EDSSLOPPY CFRMCOB*)) 
(GLOBAL '(INDXL*)) 
(PUT 'COPYEDS 'NUMBER-OF-ARGS 1) 
(PUT 'COPYEDS 'DEFINED-ON-LINE '36) 
(PUT 'COPYEDS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'COPYEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE COPYEDS (S) (APPEND S (LIST))) 
(PUT 'AUGMENT 'RTYPEFN 'GETRTYPECAR) 
(PUT 'AUGMENT 'EDSFN 'AUGMENTEDS) 
(PUT 'AUGMENTEDS 'NUMBER-OF-ARGS 2) 
(PUT 'AUGMENTEDS 'DEFINED-ON-LINE '46) 
(PUT 'AUGMENTEDS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'AUGMENTEDS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AUGMENTEDS (S U)
    (PROG ()
      (SETQ U (CONS 'LIST (GETRLIST U)))
      (SETQ U (*A2SYS U))
      (SETQ S (AUGMENTSYS S U))
      (PROG (F)
        (SETQ F (LIST 'PFAFFIAN 'CLOSED 'QUASILINEAR 'INVOLUTIVE))
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F) (REMPROPEDS S F)) (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (RETURN (CHECKEDS S)))) 
(PUT 'AUGMENTSYS 'NUMBER-OF-ARGS 2) 
(PUT 'AUGMENTSYS 'DEFINED-ON-LINE '58) 
(PUT 'AUGMENTSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'AUGMENTSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE AUGMENTSYS (S U)
    (PROG (C)
      (SETQ S (COPYEDS S))
      (SETCAR (CDR S) (SORTSYS (UNION U (CADR S)) (EDSCOB S)))
      (RETURN S))) 
(PUT 'QUASILINEAR 'PSOPFN 'QUASILINEAREVAL) 
(PUT 'QUASILINEAREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'QUASILINEAREVAL 'DEFINED-ON-LINE '71) 
(PUT 'QUASILINEAREVAL 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'QUASILINEAREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUASILINEAREVAL (S)
    (COND
     ((EDSP (SETQ S (REVAL1 (CAR S) T)))
      (COND
       ((OR (KNOWNTRUEEDS S 'QUASILINEAR)
            (AND (NOT (KNOWNFALSEEDS S 'QUASILINEAR))
                 (EDSPROTECT (LIST 'QUASILINEARP S))))
        1)
       (T 0)))
     (T (TYPERR S 'EDS)))) 
(PUT 'QUASILINEARP 'NUMBER-OF-ARGS 1) 
(PUT 'QUASILINEARP 'DEFINED-ON-LINE '80) 
(PUT 'QUASILINEARP 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'QUASILINEARP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE QUASILINEARP (S)
    (OR (KNOWNTRUEEDS S 'QUASILINEAR)
        (AND (NOT (KNOWNFALSEEDS S 'QUASILINEAR))
             (COND
              ((NOT (NORMALEDSP S))
               (RERROR 'EDS 0
                       (LIST "System not in normal form in quasilinearp")))
              ((AND (NULL (SCALARPART (CADR S)))
                    (QUASILINEARSYS (NONPFAFFPART (CADR (CLOSURE S)))
                     (PRLKRNS S)))
               (PROGN (FLAGTRUEEDS S 'QUASILINEAR) T))
              (T (PROGN (FLAGFALSEEDS S 'QUASILINEAR) NIL)))))) 
(PUT 'QUASILINEARSYS 'NUMBER-OF-ARGS 2) 
(PUT 'QUASILINEARSYS 'DEFINED-ON-LINE '94) 
(PUT 'QUASILINEARSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'QUASILINEARSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUASILINEARSYS (S PRL) (NULL (CADR (LINEARGENS S (LIST) PRL)))) 
(PUT 'LINEARGENERATORS 'NUMBER-OF-ARGS 1) 
(PUT 'LINEARGENERATORS 'DEFINED-ON-LINE '100) 
(PUT 'LINEARGENERATORS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'LINEARGENERATORS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LINEARGENERATORS (S)
    (PROG (P)
      (SETQ P (PFAFFPART (CADR S)))
      ((LAMBDA (Q) (SETQ P (APPEND P (APPEND (CAR Q) (CADR Q)))))
       (LINEARGENS (SETDIFF (CADR S) P) (LIST) (PRLKRNS S)))
      (COND ((EQUAL P (CADR S)) (RETURN S)))
      (SETQ S (COPYEDS S))
      (SETCAR (CDR S) P)
      (RETURN (SORTEDS S)))) 
(PUT 'LINEARGENS 'NUMBER-OF-ARGS 3) 
(PUT 'LINEARGENS 'DEFINED-ON-LINE '114) 
(PUT 'LINEARGENS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'LINEARGENS 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE LINEARGENS (S G PRL)
    (PROG (W XTRUNCATE* D)
      (SETQ D 0)
      (PROG (F)
        (SETQ F S)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ D (MAX D (DEGREEPF F)))
            (COND
             ((AND (NEQ (DEGREEPF F) 0) (QUASILINEARPF F PRL))
              (SETQ W (CONS F W))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ W (REVERSIP W))
      (SETQ S (SETDIFF S W))
      (COND ((NULL S) (RETURN (LIST W (LIST)))))
      (COND ((NULL W) (RETURN (LIST (LIST) S))))
      (SETQ XTRUNCATE* D)
      (SETQ G (XIDEALPF (APPEND G W)))
      (SETQ S
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F S)
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F) (COND ((SETQ F (XREDUCE F G)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F) (COND ((SETQ F (XREDUCE F G)) (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (RETURN
       ((LAMBDA (P) (LIST (APPEND W (CAR P)) (CADR P))) (LINEARGENS S G PRL))))) 
(PUT 'QUASILINEARPF 'NUMBER-OF-ARGS 2) 
(PUT 'QUASILINEARPF 'DEFINED-ON-LINE '139) 
(PUT 'QUASILINEARPF 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'QUASILINEARPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE QUASILINEARPF (F P)
    (COND ((NULL F) T)
          (T
           (AND (LEQ (LENGTH (INTERSECTION (WEDGEFAX (CAAR F)) P)) 1)
                (QUASILINEARPF (CDR F) P))))) 
(PUT 'SEMILINEAR 'PSOPFN 'SEMILINEAREVAL) 
(PUT 'SEMILINEAREVAL 'NUMBER-OF-ARGS 1) 
(PUT 'SEMILINEAREVAL 'DEFINED-ON-LINE '149) 
(PUT 'SEMILINEAREVAL 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'SEMILINEAREVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SEMILINEAREVAL (S)
    (COND
     ((EDSP (SETQ S (REVAL1 (CAR S) T)))
      (COND ((EDSPROTECT (LIST 'SEMILINEARP S)) 1) (T 0)))
     (T (TYPERR S 'EDS)))) 
(PUT 'SEMILINEARP 'NUMBER-OF-ARGS 1) 
(PUT 'SEMILINEARP 'DEFINED-ON-LINE '156) 
(PUT 'SEMILINEARP 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'SEMILINEARP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SEMILINEARP (S)
    (COND ((NOT (NORMALEDSP S)) NIL)
          (*EDSSLOPPY (EDSPROTECT (LIST 'QUASILINEARP S)))
          (T
           (SEMILINEARSYS (NONPFAFFPART (CADR (EDSPROTECT (LIST 'CLOSURE S))))
            (PRLKRNS S))))) 
(PUT 'SEMILINEARSYS 'NUMBER-OF-ARGS 2) 
(PUT 'SEMILINEARSYS 'DEFINED-ON-LINE '164) 
(PUT 'SEMILINEARSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'SEMILINEARSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SEMILINEARSYS (S PRL)
    (OR (NULL S)
        (AND (NEQ (DEGREEPF (CAR S)) 0) (SEMILINEARPF (CAR S) PRL)
             (SEMILINEARSYS (CDR S) PRL)))) 
(PUT 'SEMILINEARPF 'NUMBER-OF-ARGS 2) 
(PUT 'SEMILINEARPF 'DEFINED-ON-LINE '173) 
(PUT 'SEMILINEARPF 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'SEMILINEARPF 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SEMILINEARPF (F P)
    (OR (NULL F)
        (AND
         ((LAMBDA (L)
            (OR (EQUAL L 0)
                (AND (EQUAL L 1) (CFRMCONSTANT (CAR (CDAR F)))
                     (CFRMCONSTANT (CDR (CDAR F))))))
          (LENGTH
           (PROG (K FORALL-RESULT FORALL-ENDPTR)
             (SETQ K (WEDGEFAX (CAAR F)))
            STARTOVER
             (COND ((NULL K) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     ((LAMBDA (K) (COND ((MEMQ K P) (LIST K)))) (CAR K)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
             (SETQ K (CDR K))
             (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
            LOOPLABEL
             (COND ((NULL K) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     ((LAMBDA (K) (COND ((MEMQ K P) (LIST K)))) (CAR K)))
             (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
             (SETQ K (CDR K))
             (GO LOOPLABEL))))
         (SEMILINEARPF (CDR F) P)))) 
(PUT 'PFAFFIAN 'PSOPFN 'PFAFFIANEVAL) 
(PUT 'PFAFFIANEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'PFAFFIANEVAL 'DEFINED-ON-LINE '189) 
(PUT 'PFAFFIANEVAL 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'PFAFFIANEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PFAFFIANEVAL (S)
    (COND
     ((EDSP (SETQ S (REVAL1 (CAR S) T)))
      (COND
       ((OR (KNOWNTRUEEDS S 'PFAFFIAN)
            (AND (NOT (KNOWNFALSEEDS S 'PFAFFIAN))
                 (EDSPROTECT (LIST 'PFAFFIAN S))))
        1)
       (T 0)))
     (T (TYPERR S 'EDS)))) 
(PUT 'PFAFFIAN 'NUMBER-OF-ARGS 1) 
(PUT 'PFAFFIAN 'DEFINED-ON-LINE '198) 
(PUT 'PFAFFIAN 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'PFAFFIAN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PFAFFIAN (S)
    (OR (KNOWNTRUEEDS S 'PFAFFIAN)
        (AND (NOT (KNOWNFALSEEDS S 'PFAFFIAN))
             (COND
              ((NOT (NORMALEDSP S))
               (RERROR 'EDS 0 (LIST "System not in normal form in pfaffian")))
              ((PFAFFSYS (CADR S)) (PROGN (FLAGTRUEEDS S 'PFAFFIAN) T))
              (T (PROGN (FLAGFALSEEDS S 'PFAFFIAN) NIL)))))) 
(PUT 'PFAFFSYS 'NUMBER-OF-ARGS 1) 
(PUT 'PFAFFSYS 'DEFINED-ON-LINE '210) 
(PUT 'PFAFFSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'PFAFFSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE PFAFFSYS (S)
    (PROG (P XTRUNCATE* D)
      (SETQ D 0)
      (COND ((SCALARPART S) (RETURN NIL)))
      (PROG (F)
        (SETQ F S)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ D (MAX D (DEGREEPF F)))
            (COND ((EQUAL (DEGREEPF F) 1) (SETQ P (CONS F P))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (SETQ S (SETDIFF S P))
      (COND ((NULL S) (RETURN T)))
      (COND ((NULL P) (RETURN NIL)))
      (SETQ XTRUNCATE* D)
      (SETQ P
              (XIDEALPF
               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                 (SETQ F P)
                 (COND ((NULL F) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (F) (XREDUCE (EXDFPF F) P)) (CAR F))
                                  NIL)))
                LOOPLABEL
                 (SETQ F (CDR F))
                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (XREDUCE (EXDFPF F) P)) (CAR F))
                               NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND S (NULL (XREDUCE (CAR S) P)))) (RETURN NIL)))
        (SETQ S (CDR S))
        (GO WHILELABEL))
      (RETURN (NULL S)))) 
(PUT 'CLOSURE 'EDSFN 'CLOSURE) 
(PUT 'CLOSURE 'RTYPEFN 'GETRTYPECAR) 
(PUT 'CLOSURE 'NUMBER-OF-ARGS 1) 
(PUT 'CLOSURE 'DEFINED-ON-LINE '231) 
(PUT 'CLOSURE 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'CLOSURE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLOSURE (S)
    (PROG (P SYS S0 D)
      (SETQ D 0)
      (COND ((KNOWNTRUEEDS S 'CLOSED) (RETURN S)))
      (COND ((SETQ S0 (GETEDS S 'CLOSURE)) (RETURN S0)))
      (COND
       ((SCALARPART (CADR S))
        (LPRIM (LIST "0-forms in closure: result may not be closed"))))
      (SETQ D (LENGTH (CADDR S)))
      (SETQ P (SOLVEDPART (CADR S)))
      (SETQ SYS
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CADR S))
               STARTOVER
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (F)
                           (COND
                            ((AND (LESSP (DEGREEPF F) D)
                                  (SETQ F (XREDUCE (XREORDER (EXDFPF F)) P)))
                             (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ F (CDR F))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (F)
                           (COND
                            ((AND (LESSP (DEGREEPF F) D)
                                  (SETQ F (XREDUCE (XREORDER (EXDFPF F)) P)))
                             (LIST F))))
                         (CAR F)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ F (CDR F))
                (GO LOOPLABEL)))
      (COND ((NULL SYS) (RETURN (PROGN (FLAGTRUEEDS S 'CLOSED) S))))
      (SETQ S0 (AUGMENTSYS S SYS))
      (COND ((PFAFFPART SYS) (REMPROPEDS S0 'SOLVED)))
      (FLAGTRUEEDS S0 'CLOSED)
      (SETQ S0 (NORMALEDS S0))
      (RETURN
       (COND ((EMPTYEDSP S0) S0) ((SCALARPART (CADR S0)) S0)
             (T (PROGN (PUTEDS S 'CLOSURE S0) S0)))))) 
(FLAG '(CLOSURE) 'HIDDEN) 
(PUT 'CLOSED 'PSOPFN 'CLOSEDEVAL) 
(PUT 'CLOSEDEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'CLOSEDEVAL 'DEFINED-ON-LINE '283) 
(PUT 'CLOSEDEVAL 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'CLOSEDEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLOSEDEVAL (S)
    (COND
     ((EDSP (SETQ S (REVAL1 (CAR S) T)))
      (COND
       ((OR (KNOWNTRUEEDS S 'CLOSED)
            (AND (NOT (KNOWNFALSEEDS S 'CLOSED))
                 (EDSPROTECT (LIST 'CLOSED S))))
        1)
       (T 0)))
     ((EQCAR S 'LIST)
      (COND
       ((CLOSEDSYS
         (PROG (F FORALL-RESULT FORALL-ENDPTR)
           (SETQ F (GETRLIST S))
           (COND ((NULL F) (RETURN NIL)))
           (SETQ FORALL-RESULT
                   (SETQ FORALL-ENDPTR
                           (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F)) NIL)))
          LOOPLABEL
           (SETQ F (CDR F))
           (COND ((NULL F) (RETURN FORALL-RESULT)))
           (RPLACD FORALL-ENDPTR
                   (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F)) NIL))
           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
           (GO LOOPLABEL)))
        1)
       (T 0)))
     ((NULL (EXDFPF (XPARTITOP S))) 1) (T 0))) 
(PUT 'CLOSED 'NUMBER-OF-ARGS 1) 
(PUT 'CLOSED 'DEFINED-ON-LINE '295) 
(PUT 'CLOSED 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'CLOSED 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLOSED (S)
    (OR (KNOWNTRUEEDS S 'CLOSED)
        (AND (NOT (KNOWNFALSEEDS S 'CLOSED))
             (COND ((CLOSEDSYS (CADR S)) (PROGN (FLAGTRUEEDS S 'CLOSED) T))
                   (T (PROGN (FLAGFALSEEDS S 'CLOSED) NIL)))))) 
(PUT 'CLOSEDSYS 'NUMBER-OF-ARGS 1) 
(PUT 'CLOSEDSYS 'DEFINED-ON-LINE '305) 
(PUT 'CLOSEDSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'CLOSEDSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CLOSEDSYS (S)
    (PROG (P XTRUNCATE* D)
      (SETQ D 0)
      (PROG (F)
        (SETQ F S)
       LAB
        (COND ((NULL F) (RETURN NIL)))
        ((LAMBDA (F)
           (PROGN
            (SETQ D (MAX D (PLUS 1 (DEGREEPF F))))
            (SETQ F (XREDUCE (EXDFPF F) S))
            (COND (F (SETQ P (CONS F P))))))
         (CAR F))
        (SETQ F (CDR F))
        (GO LAB))
      (COND ((NULL P) (RETURN T)))
      (SETQ XTRUNCATE* D)
      (SETQ S (XIDEALPF S))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND P (NULL (XREDUCE (CAR P) S)))) (RETURN NIL)))
        (SETQ P (CDR P))
        (GO WHILELABEL))
      (RETURN (NULL P)))) 
(FLAG '(FROBENIUS) 'OPFN) 
(PUT 'FROBENIUS 'NUMBER-OF-ARGS 1) 
(PUT 'FROBENIUS 'DEFINED-ON-LINE '321) 
(PUT 'FROBENIUS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'FROBENIUS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FROBENIUS (U)
    (COND
     ((EDSP U)
      (AND (NULL (NONPFAFFPART (CADR U)))
           (NULL (NONPFAFFPART (CADR (EDSPROTECT (LIST 'CLOSURE U)))))))
     ((EQCAR U 'LIST)
      (FROBENIUSSYS
       (PROG (F FORALL-RESULT FORALL-ENDPTR)
         (SETQ F (GETRLIST U))
         (COND ((NULL F) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F)) NIL)))
        LOOPLABEL
         (SETQ F (CDR F))
         (COND ((NULL F) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR (CONS ((LAMBDA (F) (XPARTITOP F)) (CAR F)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))
     (T (RERROR 'EDS 0 "Invalid argument to frobenius")))) 
(FLAG '(FROBENIUS) 'BOOLEAN) 
(PUT 'FROBENIUSSYS 'NUMBER-OF-ARGS 1) 
(PUT 'FROBENIUSSYS 'DEFINED-ON-LINE '335) 
(PUT 'FROBENIUSSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'FROBENIUSSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE FROBENIUSSYS (S)
    (PROG (P)
      (SETQ P (PFAFFPART S))
      (SETQ S
              (UNION
               (PROG (F FORALL-RESULT FORALL-ENDPTR)
                 (SETQ F P)
                 (COND ((NULL F) (RETURN NIL)))
                 (SETQ FORALL-RESULT
                         (SETQ FORALL-ENDPTR
                                 (CONS ((LAMBDA (F) (EXDFPF F)) (CAR F)) NIL)))
                LOOPLABEL
                 (SETQ F (CDR F))
                 (COND ((NULL F) (RETURN FORALL-RESULT)))
                 (RPLACD FORALL-ENDPTR
                         (CONS ((LAMBDA (F) (EXDFPF F)) (CAR F)) NIL))
                 (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                 (GO LOOPLABEL))
               (SETDIFF S P)))
      (SETQ P (XAUTOREDUCE P))
      (PROG ()
       WHILELABEL
        (COND ((NOT (AND S (NULL (XREDUCE (CAR S) P)))) (RETURN NIL)))
        (SETQ S (CDR S))
        (GO WHILELABEL))
      (RETURN (NULL S)))) 
(PUT 'CAUCHY_SYSTEM 'RTYPEFN 'QUOTELIST) 
(PUT 'CAUCHY_SYSTEM 'LISTFN 'EVALCAUCHYSYS) 
(PUT 'EVALCAUCHYSYS 'NUMBER-OF-ARGS 2) 
(PUT 'EVALCAUCHYSYS 'DEFINED-ON-LINE '349) 
(PUT 'EVALCAUCHYSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'EVALCAUCHYSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALCAUCHYSYS (U V)
    (COND
     ((XEDSP (SETQ U (REVAL1 (CAR U) T)))
      (EVALCARTANSYS (LIST (EDSPROTECT (LIST 'CLOSURE U))) V))
     ((EQCAR U 'LIST)
      (EVALCARTANSYS
       (LIST
        (APPEND U
                (PROG (F FORALL-RESULT FORALL-ENDPTR)
                  (SETQ F (CDR U))
                  (COND ((NULL F) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (F) (REVAL1 (LIST 'D F) NIL))
                                    (CAR F))
                                   NIL)))
                 LOOPLABEL
                  (SETQ F (CDR F))
                  (COND ((NULL F) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (F) (REVAL1 (LIST 'D F) NIL)) (CAR F))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))
       V))
     (T
      (EVALCARTANSYS (LIST (CONS 'LIST (LIST U (REVAL1 (LIST 'D U) NIL)))) V)))) 
(PUT 'CARTAN_SYSTEM 'RTYPEFN 'QUOTELIST) 
(PUT 'CARTAN_SYSTEM 'LISTFN 'EVALCARTANSYS) 
(PUT 'EVALCARTANSYS 'NUMBER-OF-ARGS 2) 
(PUT 'EVALCARTANSYS 'DEFINED-ON-LINE '362) 
(PUT 'EVALCARTANSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'EVALCARTANSYS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE EVALCARTANSYS (U V)
    (COND
     ((XEDSP (SETQ U (REVAL1 (CAR U) T)))
      (COND ((EDSP U) (*SYS2A1 (EDSPROTECT (LIST 'CARTANSYSEDS U)) V))
            (T
             (CONS 'LIST
                   (PROG (S FORALL-RESULT FORALL-ENDPTR)
                     (SETQ S (CDR U))
                     (COND ((NULL S) (RETURN NIL)))
                     (SETQ FORALL-RESULT
                             (SETQ FORALL-ENDPTR
                                     (CONS
                                      ((LAMBDA (S)
                                         (*SYS2A1
                                          (EDSPROTECT (LIST 'CARTANSYSEDS U))
                                          V))
                                       (CAR S))
                                      NIL)))
                    LOOPLABEL
                     (SETQ S (CDR S))
                     (COND ((NULL S) (RETURN FORALL-RESULT)))
                     (RPLACD FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (S)
                                 (*SYS2A1 (EDSPROTECT (LIST 'CARTANSYSEDS U))
                                  V))
                               (CAR S))
                              NIL))
                     (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                     (GO LOOPLABEL))))))
     ((EQCAR U 'LIST) (*SYS2A1 (CARTANSYS (*A2SYS U)) V))
     (T (*SYS2A1 (CARTANSYSPF (XPARTITOP U)) V)))) 
(PUT 'CARTANSYS 'NUMBER-OF-ARGS 1) 
(PUT 'CARTANSYS 'DEFINED-ON-LINE '374) 
(PUT 'CARTANSYS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'CARTANSYS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CARTANSYS (U)
    (PROG (XTRUNCATE*)
      (SETQ XTRUNCATE*
              (EVAL
               (CONS 'MAX
                     (PROG (F FORALL-RESULT FORALL-ENDPTR)
                       (SETQ F U)
                       (COND ((NULL F) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (F) (DEGREEPF F)) (CAR F))
                                        NIL)))
                      LOOPLABEL
                       (SETQ F (CDR F))
                       (COND ((NULL F) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (F) (DEGREEPF F)) (CAR F)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ XTRUNCATE* (DIFFERENCE XTRUNCATE* 1))
      (SETQ U (XIDEALPF U))
      (RETURN
       (REVERSIP
        (XAUTOREDUCE
         (PURGE
          (PROG (F FORALL-RESULT FORALL-ENDPTR)
            (SETQ F U)
           STARTOVER
            (COND ((NULL F) (RETURN NIL)))
            (SETQ FORALL-RESULT ((LAMBDA (F) (CARTANSYSPF F)) (CAR F)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
            (SETQ F (CDR F))
            (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
           LOOPLABEL
            (COND ((NULL F) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR ((LAMBDA (F) (CARTANSYSPF F)) (CAR F)))
            (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
            (SETQ F (CDR F))
            (GO LOOPLABEL)))))))) 
(PUT 'CARTANSYSPF 'NUMBER-OF-ARGS 1) 
(PUT 'CARTANSYSPF 'DEFINED-ON-LINE '384) 
(PUT 'CARTANSYSPF 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'CARTANSYSPF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CARTANSYSPF (F)
    (PROG (X P Q Z)
      (COND ((EQUAL (DEGREEPF F) 1) (RETURN (LIST F))))
      (PROG ()
       WHILELABEL
        (COND ((NOT F) (RETURN NIL)))
        (PROG ()
          (SETQ P (WEDGEFAX (CAAR F)))
          (PROG (K)
            (SETQ K P)
           LAB
            (COND ((NULL K) (RETURN NIL)))
            ((LAMBDA (K)
               (COND
                ((NOT (MEMBER (SETQ Q (DELETE K P)) Z))
                 (PROGN (SETQ Z (CONS Q Z)) (SETQ X (CONS (XCOEFF F Q) X))))))
             (CAR K))
            (SETQ K (CDR K))
            (GO LAB))
          (SETQ F (CDR F)))
        (GO WHILELABEL))
      (RETURN (REVERSE (XAUTOREDUCE (PURGE X)))))) 
(PUT 'CARTANSYSEDS 'NUMBER-OF-ARGS 1) 
(PUT 'CARTANSYSEDS 'DEFINED-ON-LINE '401) 
(PUT 'CARTANSYSEDS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'CARTANSYSEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CARTANSYSEDS (S) (CARTANSYS (CADR S))) 
(PUT 'LINEARISE 'EDSFN 'LINEARISEEDS) 
(PUT 'LINEARISE 'RTYPEFN 'QUOTEEDS) 
(PUT 'LINEARIZE 'EDSFN 'LINEARISEEDS) 
(PUT 'LINEARIZE 'RTYPEFN 'QUOTEEDS) 
(FLAG '(LINEARISE LINEARIZE) 'NOSPREAD) 
(PUT 'LINEARISEEDS 'NUMBER-OF-ARGS 1) 
(PUT 'LINEARISEEDS 'DEFINED-ON-LINE '412) 
(PUT 'LINEARISEEDS 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'LINEARISEEDS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LINEARISEEDS (U)
    (PROG (X)
      (COND
       ((OR (NULL U) (GREATERP (LENGTH U) 2))
        (RERROR 'EDS 0 (LIST "Wrong number of arguments to linearise"))))
      (COND ((CDR U) (SETQ X (*A2SYS (CADR U)))))
      (COND ((NONPFAFFPART X) (TYPERR (CADR U) "integral element")))
      (RETURN (EDSPROTECT (LIST 'LINEARISE (CAR U) X))))) 
(PUT 'LINEARISE 'NUMBER-OF-ARGS 2) 
(PUT 'LINEARISE 'DEFINED-ON-LINE '423) 
(PUT 'LINEARISE 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'LINEARISE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LINEARISE (S X)
    (COND ((QUASILINEARP S) (LINEARGENERATORS S))
          (T
           (PROG (XX Q PRL)
             (SETQ S (COPYEDS (CLOSURE S)))
             (SETQ X (XREORDERSYS X))
             (SETQ Q (NONPFAFFPART (CADR S)))
             (SETQ PRL (PRLKRNS S))
             (SETQ XX
                     (PURGE
                      (PROG (F FORALL-RESULT FORALL-ENDPTR)
                        (SETQ F Q)
                       STARTOVER
                        (COND ((NULL F) (RETURN NIL)))
                        (SETQ FORALL-RESULT
                                ((LAMBDA (F)
                                   (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ K (XPOWS F))
                                    STARTOVER
                                     (COND ((NULL K) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             ((LAMBDA (K)
                                                (NONLINFAX
                                                 (INTERSECTION (WEDGEFAX K)
                                                               PRL)))
                                              (CAR K)))
                                     (SETQ FORALL-ENDPTR
                                             (LASTPAIR FORALL-RESULT))
                                     (SETQ K (CDR K))
                                     (COND
                                      ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                    LOOPLABEL
                                     (COND ((NULL K) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             ((LAMBDA (K)
                                                (NONLINFAX
                                                 (INTERSECTION (WEDGEFAX K)
                                                               PRL)))
                                              (CAR K)))
                                     (SETQ FORALL-ENDPTR
                                             (LASTPAIR FORALL-ENDPTR))
                                     (SETQ K (CDR K))
                                     (GO LOOPLABEL)))
                                 (CAR F)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                        (SETQ F (CDR F))
                        (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                       LOOPLABEL
                        (COND ((NULL F) (RETURN FORALL-RESULT)))
                        (RPLACD FORALL-ENDPTR
                                ((LAMBDA (F)
                                   (PROG (K FORALL-RESULT FORALL-ENDPTR)
                                     (SETQ K (XPOWS F))
                                    STARTOVER
                                     (COND ((NULL K) (RETURN NIL)))
                                     (SETQ FORALL-RESULT
                                             ((LAMBDA (K)
                                                (NONLINFAX
                                                 (INTERSECTION (WEDGEFAX K)
                                                               PRL)))
                                              (CAR K)))
                                     (SETQ FORALL-ENDPTR
                                             (LASTPAIR FORALL-RESULT))
                                     (SETQ K (CDR K))
                                     (COND
                                      ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                                    LOOPLABEL
                                     (COND ((NULL K) (RETURN FORALL-RESULT)))
                                     (RPLACD FORALL-ENDPTR
                                             ((LAMBDA (K)
                                                (NONLINFAX
                                                 (INTERSECTION (WEDGEFAX K)
                                                               PRL)))
                                              (CAR K)))
                                     (SETQ FORALL-ENDPTR
                                             (LASTPAIR FORALL-ENDPTR))
                                     (SETQ K (CDR K))
                                     (GO LOOPLABEL)))
                                 (CAR F)))
                        (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                        (SETQ F (CDR F))
                        (GO LOOPLABEL))))
             (SETQ X (PAIR (LPOWS X) X))
             (SETQ XX
                     (PROG (PR FORALL-RESULT FORALL-ENDPTR)
                       (SETQ PR XX)
                       (COND ((NULL PR) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (PR)
                                           (WEDGEPF (CDR (ATSOC (CAR PR) X))
                                                    (CDR (ATSOC (CADR PR) X))))
                                         (CAR PR))
                                        NIL)))
                      LOOPLABEL
                       (SETQ PR (CDR PR))
                       (COND ((NULL PR) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (PR)
                                   (WEDGEPF (CDR (ATSOC (CAR PR) X))
                                            (CDR (ATSOC (CADR PR) X))))
                                 (CAR PR))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))
             (SETCAR (CDR S)
                     (APPEND (SETDIFF (CADR S) Q)
                             (PROG (F FORALL-RESULT FORALL-ENDPTR)
                               (SETQ F Q)
                              STARTOVER
                               (COND ((NULL F) (RETURN NIL)))
                               (SETQ FORALL-RESULT
                                       ((LAMBDA (F)
                                          (COND
                                           ((SETQ F (XREDUCE F XX)) (LIST F))))
                                        (CAR F)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                               (SETQ F (CDR F))
                               (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                              LOOPLABEL
                               (COND ((NULL F) (RETURN FORALL-RESULT)))
                               (RPLACD FORALL-ENDPTR
                                       ((LAMBDA (F)
                                          (COND
                                           ((SETQ F (XREDUCE F XX)) (LIST F))))
                                        (CAR F)))
                               (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                               (SETQ F (CDR F))
                               (GO LOOPLABEL))))
             (FLAGTRUEEDS S 'QUASILINEAR)
             (RETURN S))))) 
(PUT 'NONLINFAX 'NUMBER-OF-ARGS 1) 
(PUT 'NONLINFAX 'DEFINED-ON-LINE '449) 
(PUT 'NONLINFAX 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'NONLINFAX 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE NONLINFAX (L)
    (COND
     ((GREATERP (LENGTH L) 1)
      (CONS (LIST (CAR L) (CADR L)) (NONLINFAX (CDDR L)))))) 
(PUT 'ONE_FORMS 'RTYPEFN 'QUOTELIST) 
(PUT 'ONE_FORMS 'LISTFN 'ONEFORMSEVAL) 
(PUT 'ONEFORMSEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'ONEFORMSEVAL 'DEFINED-ON-LINE '497) 
(PUT 'ONEFORMSEVAL 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'ONEFORMSEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ONEFORMSEVAL (U V)
    (COND ((EDSP (SETQ U (REVAL1 (CAR U) T))) (*SYS2A1 (PFAFFPART (CADR U)) V))
          ((XEDSP U)
           (CONS 'LIST
                 (PROG (S FORALL-RESULT FORALL-ENDPTR)
                   (SETQ S (GETRLIST U))
                   (COND ((NULL S) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (S)
                                       (*SYS2A1 (PFAFFPART (CADR S)) V))
                                     (CAR S))
                                    NIL)))
                  LOOPLABEL
                   (SETQ S (CDR S))
                   (COND ((NULL S) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS
                            ((LAMBDA (S) (*SYS2A1 (PFAFFPART (CADR S)) V))
                             (CAR S))
                            NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T
           (CONS 'LIST
                 (PROG (F FORALL-RESULT FORALL-ENDPTR)
                   (SETQ F (GETRLIST U))
                  STARTOVER
                   (COND ((NULL F) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           ((LAMBDA (F)
                              (COND
                               ((EQUAL (REVAL1 (LIST 'EXDEGREE F) T) 1)
                                (LIST F))))
                            (CAR F)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                   (SETQ F (CDR F))
                   (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                  LOOPLABEL
                   (COND ((NULL F) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           ((LAMBDA (F)
                              (COND
                               ((EQUAL (REVAL1 (LIST 'EXDEGREE F) T) 1)
                                (LIST F))))
                            (CAR F)))
                   (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                   (SETQ F (CDR F))
                   (GO LOOPLABEL)))))) 
(PUT 'ZERO_FORMS 'RTYPEFN 'QUOTELIST) 
(PUT 'ZERO_FORMS 'LISTFN 'ZEROFORMSEVAL) 
(PUT 'NOUGHT_FORMS 'RTYPEFN 'QUOTELIST) 
(PUT 'NOUGHT_FORMS 'LISTFN 'ZEROFORMSEVAL) 
(PUT 'ZEROFORMSEVAL 'NUMBER-OF-ARGS 2) 
(PUT 'ZEROFORMSEVAL 'DEFINED-ON-LINE '514) 
(PUT 'ZEROFORMSEVAL 'DEFINED-IN-FILE 'EDS/SYSTEMS.RED) 
(PUT 'ZEROFORMSEVAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE ZEROFORMSEVAL (U V)
    (COND
     ((EDSP (SETQ U (REVAL1 (CAR U) T))) (*SYS2A1 (SCALARPART (CADR U)) V))
     ((XEDSP U)
      (CONS 'LIST
            (PROG (S FORALL-RESULT FORALL-ENDPTR)
              (SETQ S (GETRLIST U))
              (COND ((NULL S) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      (SETQ FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (S) (*SYS2A1 (SCALARPART (CADR S)) V))
                                (CAR S))
                               NIL)))
             LOOPLABEL
              (SETQ S (CDR S))
              (COND ((NULL S) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (S) (*SYS2A1 (SCALARPART (CADR S)) V)) (CAR S))
                       NIL))
              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
              (GO LOOPLABEL))))
     (T
      (CONS 'LIST
            (PROG (F FORALL-RESULT FORALL-ENDPTR)
              (SETQ F (GETRLIST U))
             STARTOVER
              (COND ((NULL F) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      ((LAMBDA (F)
                         (COND
                          ((EQUAL (REVAL1 (LIST 'EXDEGREE F) T) 0) (LIST F))))
                       (CAR F)))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
              (SETQ F (CDR F))
              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
             LOOPLABEL
              (COND ((NULL F) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      ((LAMBDA (F)
                         (COND
                          ((EQUAL (REVAL1 (LIST 'EXDEGREE F) T) 0) (LIST F))))
                       (CAR F)))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
              (SETQ F (CDR F))
              (GO LOOPLABEL)))))) 
(ENDMODULE) 