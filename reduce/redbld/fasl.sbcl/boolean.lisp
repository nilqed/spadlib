(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'BOOLEAN)) 
(CREATE-PACKAGE '(BOOLEAN) '(CONTRIB MISC)) 
(AEVAL (OPERATOR (LIST 'PROP* 'NOT_PROP*))) 
(MKOP 'IMPLIES) 
(INFIX (LIST 'IMPLIES)) 
(MKOP 'EQUIV) 
(INFIX (LIST 'EQUIV)) 
(AEVAL (PRECEDENCE (LIST 'EQUIV 'REPLACEBY))) 
(AEVAL (PRECEDENCE (LIST 'IMPLIES 'EQUIV))) 
(AEVAL
 (LET
  '((REPLACEBY (TIMES (PROP* (~ X)) (PROP* X)) (PROP* X))
    (REPLACEBY (TIMES (NOT_PROP* (~ X)) (NOT_PROP* X)) (NOT_PROP* X))
    (REPLACEBY (TIMES (PROP* (~ X)) (NOT_PROP* X)) 0)))) 
(FLUID '(PROPVARS* |'AND| |'OR| |'TRUE| |'FALSE|)) 
(PUT 'SIMP-PROP 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-PROP 'DEFINED-ON-LINE '53) 
(PUT 'SIMP-PROP 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'SIMP-PROP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-PROP (U)
    (PROG (PROPVARS* W OPT)
      (SETQ OPT
              (PROG (F FORALL-RESULT FORALL-ENDPTR)
                (SETQ F (CDR U))
                (COND ((NULL F) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (F) (REVAL1 F T)) (CAR F))
                                      NIL)))
               LOOPLABEL
                (SETQ F (CDR F))
                (COND ((NULL F) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (F) (REVAL1 F T)) (CAR F)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND
       ((MEMBER 'AND OPT)
        (PROGN
         (SETQ |'AND| 'OR)
         (SETQ |'OR| 'AND)
         (SETQ |'TRUE| 0)
         (SETQ |'FALSE| 1)
         NIL))
       (T
        (PROGN
         (SETQ |'AND| 'AND)
         (SETQ |'OR| 'OR)
         (SETQ |'TRUE| 1)
         (SETQ |'FALSE| 0)
         NIL)))
      (SETQ W (REVAL1 (PREPF (SIMP-PROP1 (CAR U) T)) T))
      (COND ((EQUAL W 0) (RETURN (SIMP |'FALSE|))))
      (PROG (X)
        (SETQ X PROPVARS*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (SETQ W
                   (REVAL1
                    (LIST 'TIMES W
                          (PREPF (SIMP-PROP1 (LIST |'OR| X (LIST 'NOT X)) T)))
                    T)))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (SETQ W (SIMP-PROP-DIST W))
      (COND ((NOT (MEMBER 'FULL OPT)) (SETQ W (SIMP-PROP2 W))))
      (SETQ W (SIMP-PROP-FORM W))
      (COND ((NUMBERP W) (RETURN (CONS W 1))))
      (COND ((NOT (ATOM W)) (SETQ W (LIST 'BOOLEAN W))))
      (RETURN (CONS (CONS (CONS (CONS W 1) 1) NIL) 1)))) 
(PUT 'BOOLEAN 'SIMPFN 'SIMP-PROP) 
(PUT 'SIMP-PROP1 'NUMBER-OF-ARGS 2) 
(PUT 'SIMP-PROP1 'DEFINED-ON-LINE '77) 
(PUT 'SIMP-PROP1 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'SIMP-PROP1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMP-PROP1 (U M)
    (PROG (W)
      (COND ((ATOM U) (GO Z)))
      (COND
       ((OR (AND (EQUAL (CAR U) |'AND|) M) (AND (EQUAL (CAR U) |'OR|) (NOT M)))
        (PROGN
         (SETQ W 1)
         (PROG (Q)
           (SETQ Q (CDR U))
          LAB
           (COND ((NULL Q) (RETURN NIL)))
           ((LAMBDA (Q)
              (SETQ W
                      ((LAMBDA (G539)
                         (COND (*PHYSOP-LOADED (PHYSOP-MULTF W G539))
                               (T (POLY-MULTF W G539))))
                       (SIMP-PROP1 Q M))))
            (CAR Q))
           (SETQ Q (CDR Q))
           (GO LAB))))
       ((OR (AND (EQUAL (CAR U) |'OR|) M) (AND (EQUAL (CAR U) |'AND|) (NOT M)))
        (PROGN
         (SETQ W NIL)
         (PROG (Q)
           (SETQ Q (CDR U))
          LAB
           (COND ((NULL Q) (RETURN NIL)))
           ((LAMBDA (Q) (SETQ W (ADDF W (SIMP-PROP1 Q M)))) (CAR Q))
           (SETQ Q (CDR Q))
           (GO LAB))))
       ((EQUAL (CAR U) 'NOT) (SETQ W (SIMP-PROP1 (CADR U) (NOT M))))
       ((EQUAL (CAR U) 'IMPLIES)
        (COND
         (M (SETQ W (SIMP-PROP1 (LIST 'OR (LIST 'NOT (CADR U)) (CADDR U)) T)))
         (T
          (SETQ W (SIMP-PROP1 (LIST 'OR (LIST 'NOT (CADDR U)) (CADR U)) T)))))
       ((EQUAL (CAR U) 'EQUIV)
        (SETQ W
                (SIMP-PROP1
                 (LIST 'OR (LIST 'AND (CADR U) (CADDR U))
                       (LIST 'AND (LIST 'NOT (CADR U)) (LIST 'NOT (CADDR U))))
                 M)))
       (T (GO Z1)))
      (RETURN W)
     Z
      (COND ((OR (EQUAL U 1) (EQUAL U T) (EQUAL U 'TRUE)) (SETQ U M))
            ((OR (EQUAL U 0) (EQUAL U NIL) (EQUAL U 'FALSE)) (SETQ U (NOT M))))
      (COND ((EQUAL U T) (RETURN (SIMP-PROP1 '(OR *TRUE (NOT *TRUE)) T))))
      (COND ((EQUAL U NIL) (RETURN (SIMP-PROP1 '(AND *TRUE (NOT *TRUE)) T))))
     Z1
      (SETQ U (REVAL1 U T))
      (COND ((EQCAR U 'BOOLEAN) (RETURN (SIMP-PROP1 (CADR U) M))))
      (SETQ W (CAR (SIMP (LIST (COND (M 'PROP*) (T 'NOT_PROP*)) U))))
      (COND ((NOT (MEMBER U PROPVARS*)) (SETQ PROPVARS* (CONS U PROPVARS*))))
      (RETURN W))) 
(PUT 'SIMP-PROP2 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-PROP2 'DEFINED-ON-LINE '109) 
(PUT 'SIMP-PROP2 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'SIMP-PROP2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-PROP2 (W)
    (PROG (Y Z O Q1 Q2 TERM OLD)
      (PROG (X)
        (SETQ X PROPVARS*)
       LAB
        (COND ((NULL X) (RETURN NIL)))
        ((LAMBDA (X)
           (PROGN
            (SETQ OLD NIL)
            (PROG ()
             WHILELABEL
              (COND ((NOT W) (RETURN NIL)))
              (PROGN
               (SETQ TERM (CAR W))
               (SETQ W (CDR W))
               (SETQ Q1 (LIST 'PROP* X))
               (SETQ Q2 (LIST 'NOT_PROP* X))
               (COND
                ((NOT (MEMBER Q1 TERM))
                 (PROGN (SETQ Y Q2) (SETQ Q2 Q1) (SETQ Q1 Y))))
               (SETQ Z (SUBST Q2 Q1 TERM))
               (SETQ OLD (CONS TERM OLD))
               (COND
                ((SETQ O (MEMBER Z W))
                 (PROGN
                  (COND
                   (O
                    (PROGN
                     (SETQ W (DELETE (CAR O) W))
                     (SETQ OLD (CONS (CAR O) OLD)))))
                  (SETQ TERM (DELETE Q1 TERM))
                  (SETQ OLD (UNION (LIST TERM) OLD))
                  NIL)))
               NIL)
              (GO WHILELABEL))
            (SETQ W OLD)
            NIL))
         (CAR X))
        (SETQ X (CDR X))
        (GO LAB))
      (RETURN (SIMP-PROP-CONDENSE W)))) 
(PUT 'SIMP-PROP-CONDENSE 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-PROP-CONDENSE 'DEFINED-ON-LINE '131) 
(PUT 'SIMP-PROP-CONDENSE 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'SIMP-PROP-CONDENSE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-PROP-CONDENSE (U)
    (PROG (W R)
      (SETQ U
              (SORT U
                    (FUNCTION
                     (LAMBDA (V1 V2) (LESSP (LENGTH V1) (LENGTH V2))))))
      (PROG ()
       WHILELABEL
        (COND ((NOT U) (RETURN NIL)))
        (PROGN
         (SETQ W (CAR U))
         (SETQ U (CDR U))
         (SETQ R (CONS W R))
         (PROG (Q)
           (SETQ Q U)
          LAB
           (COND ((NULL Q) (RETURN NIL)))
           ((LAMBDA (Q) (COND ((SUBSETP W Q) (SETQ U (DELETE Q U))))) (CAR Q))
           (SETQ Q (CDR Q))
           (GO LAB))
         NIL)
        (GO WHILELABEL))
      (RETURN (ORDN R)))) 
(PUT 'SIMP-PROP-DIST 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-PROP-DIST 'DEFINED-ON-LINE '142) 
(PUT 'SIMP-PROP-DIST 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'SIMP-PROP-DIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-PROP-DIST (W)
    (PROGN
     (COND ((EQCAR W 'PLUS) (SETQ W (CDR W))) (T (SETQ W (LIST W))))
     (SETQ W
             (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
               (SETQ TERM W)
               (COND ((NULL TERM) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (TERM)
                                   (PROGN
                                    (SETQ TERM
                                            (COND
                                             ((EQCAR TERM 'TIMES) (CDR TERM))
                                             (T (LIST TERM))))
                                    (COND
                                     ((NUMBERP (CAR TERM))
                                      (SETQ TERM (CDR TERM))))
                                    (SORT TERM
                                          (FUNCTION
                                           (LAMBDA (P1 P2)
                                             (ORDP (CADR P1) (CADR P2)))))))
                                 (CAR TERM))
                                NIL)))
              LOOPLABEL
               (SETQ TERM (CDR TERM))
               (COND ((NULL TERM) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (TERM)
                           (PROGN
                            (SETQ TERM
                                    (COND ((EQCAR TERM 'TIMES) (CDR TERM))
                                          (T (LIST TERM))))
                            (COND
                             ((NUMBERP (CAR TERM)) (SETQ TERM (CDR TERM))))
                            (SORT TERM
                                  (FUNCTION
                                   (LAMBDA (P1 P2)
                                     (ORDP (CADR P1) (CADR P2)))))))
                         (CAR TERM))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))
     (SORT W (FUNCTION SIMP-PROP-ORDER)))) 
(PUT 'SIMP-PROP-ORDER 'NUMBER-OF-ARGS 2) 
(PUT 'SIMP-PROP-ORDER 'DEFINED-ON-LINE '153) 
(PUT 'SIMP-PROP-ORDER 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'SIMP-PROP-ORDER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE SIMP-PROP-ORDER (A B)
    (COND ((NULL A) NIL)
          ((EQUAL (CAAR A) (CAAR B)) (SIMP-PROP-ORDER (CDR A) (CDR B)))
          ((EQUAL (CAAR A) 'PROP*) T) (T NIL))) 
(PUT 'SIMP-PROP-FORM 'NUMBER-OF-ARGS 1) 
(PUT 'SIMP-PROP-FORM 'DEFINED-ON-LINE '158) 
(PUT 'SIMP-PROP-FORM 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'SIMP-PROP-FORM 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMP-PROP-FORM (U)
    (COND ((EQUAL U '(NIL)) |'TRUE|)
          (T
           (PROGN
            (SETQ U
                    (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
                      (SETQ TERM U)
                      (COND ((NULL TERM) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              (SETQ FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (TERM)
                                          (PROGN
                                           (SETQ TERM
                                                   (PROG (X FORALL-RESULT
                                                          FORALL-ENDPTR)
                                                     (SETQ X TERM)
                                                     (COND
                                                      ((NULL X) (RETURN NIL)))
                                                     (SETQ FORALL-RESULT
                                                             (SETQ FORALL-ENDPTR
                                                                     (CONS
                                                                      ((LAMBDA
                                                                           (X)
                                                                         (COND
                                                                          ((EQCAR
                                                                            X
                                                                            'NOT_PROP*)
                                                                           (LIST
                                                                            'NOT
                                                                            (CADR
                                                                             X)))
                                                                          (T
                                                                           (CADR
                                                                            X))))
                                                                       (CAR X))
                                                                      NIL)))
                                                    LOOPLABEL
                                                     (SETQ X (CDR X))
                                                     (COND
                                                      ((NULL X)
                                                       (RETURN FORALL-RESULT)))
                                                     (RPLACD FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (X)
                                                                 (COND
                                                                  ((EQCAR X
                                                                          'NOT_PROP*)
                                                                   (LIST 'NOT
                                                                         (CADR
                                                                          X)))
                                                                  (T
                                                                   (CADR X))))
                                                               (CAR X))
                                                              NIL))
                                                     (SETQ FORALL-ENDPTR
                                                             (CDR
                                                              FORALL-ENDPTR))
                                                     (GO LOOPLABEL)))
                                           (COND
                                            ((CDR TERM) (CONS |'AND| TERM))
                                            (T (CAR TERM)))))
                                        (CAR TERM))
                                       NIL)))
                     LOOPLABEL
                      (SETQ TERM (CDR TERM))
                      (COND ((NULL TERM) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              (CONS
                               ((LAMBDA (TERM)
                                  (PROGN
                                   (SETQ TERM
                                           (PROG (X FORALL-RESULT
                                                  FORALL-ENDPTR)
                                             (SETQ X TERM)
                                             (COND ((NULL X) (RETURN NIL)))
                                             (SETQ FORALL-RESULT
                                                     (SETQ FORALL-ENDPTR
                                                             (CONS
                                                              ((LAMBDA (X)
                                                                 (COND
                                                                  ((EQCAR X
                                                                          'NOT_PROP*)
                                                                   (LIST 'NOT
                                                                         (CADR
                                                                          X)))
                                                                  (T
                                                                   (CADR X))))
                                                               (CAR X))
                                                              NIL)))
                                            LOOPLABEL
                                             (SETQ X (CDR X))
                                             (COND
                                              ((NULL X)
                                               (RETURN FORALL-RESULT)))
                                             (RPLACD FORALL-ENDPTR
                                                     (CONS
                                                      ((LAMBDA (X)
                                                         (COND
                                                          ((EQCAR X 'NOT_PROP*)
                                                           (LIST 'NOT
                                                                 (CADR X)))
                                                          (T (CADR X))))
                                                       (CAR X))
                                                      NIL))
                                             (SETQ FORALL-ENDPTR
                                                     (CDR FORALL-ENDPTR))
                                             (GO LOOPLABEL)))
                                   (COND ((CDR TERM) (CONS |'AND| TERM))
                                         (T (CAR TERM)))))
                                (CAR TERM))
                               NIL))
                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                      (GO LOOPLABEL)))
            (COND ((CDR U) (CONS |'OR| U)) (T (CAR U))))))) 
(FLUID '(BOOL-BREAK*)) 
(PUT 'TEST-BOOL 'NUMBER-OF-ARGS 1) 
(PUT 'TEST-BOOL 'DEFINED-ON-LINE '177) 
(PUT 'TEST-BOOL 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'TEST-BOOL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TEST-BOOL (U) (MK*SQ (SIMP-PROP (LIST (BOOLEAN-EVAL1 (CAR U)))))) 
(PUT 'TESTBOOL 'PSOPFN 'TEST-BOOL) 
(PUT 'BOOLEAN-EVAL1 'NUMBER-OF-ARGS 1) 
(PUT 'BOOLEAN-EVAL1 'DEFINED-ON-LINE '182) 
(PUT 'BOOLEAN-EVAL1 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'BOOLEAN-EVAL1 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BOOLEAN-EVAL1 (U)
    (PROG (V)
      (RETURN
       (COND
        ((AND (EQCAR U 'SQ*) (CDDR U)
              (EQCAR (SETQ V (PRESPSQ (CADR U))) 'BOOLEAN))
         (BOOLEAN-EVAL2 (CADR V)))
        (T (BOOLEAN-EVAL2 (PREPF (CAR (SIMP-PROP (LIST U)))))))))) 
(PUT 'BOOLEAN-EVAL2 'NUMBER-OF-ARGS 1) 
(PUT 'BOOLEAN-EVAL2 'DEFINED-ON-LINE '190) 
(PUT 'BOOLEAN-EVAL2 'DEFINED-IN-FILE 'MISC/BOOLEAN.RED) 
(PUT 'BOOLEAN-EVAL2 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE BOOLEAN-EVAL2 (U)
    (COND ((EQCAR U 'BOOLEAN) (BOOLEAN-EVAL2 (CADR U)))
          ((OR (EQCAR U 'AND) (EQCAR U 'OR) (EQCAR U 'NOT))
           (CONS (CAR U)
                 (PROG (X FORALL-RESULT FORALL-ENDPTR)
                   (SETQ X (CDR U))
                   (COND ((NULL X) (RETURN NIL)))
                   (SETQ FORALL-RESULT
                           (SETQ FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (X) (BOOLEAN-EVAL2 X)) (CAR X))
                                    NIL)))
                  LOOPLABEL
                   (SETQ X (CDR X))
                   (COND ((NULL X) (RETURN FORALL-RESULT)))
                   (RPLACD FORALL-ENDPTR
                           (CONS ((LAMBDA (X) (BOOLEAN-EVAL2 X)) (CAR X)) NIL))
                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                   (GO LOOPLABEL))))
          (T
           ((LAMBDA (R)
              (PROGN
               ((LAMBDA (*PROTFG)
                  (SETQ R (ERRORSET (FORMBOOL U NIL 'ALGEBRAIC) NIL NIL)))
                T)
               (COND
                ((ERRORP R) (PROGN (SETQ BOOL-BREAK* T) (SETQ ERFG* NIL) U))
                (T (CAR R)))))
            NIL)))) 
(PUT 'AND 'PRTCH "/\\") 
(PUT 'OR 'PRTCH " \\/ ") 
(ENDMODULE) 