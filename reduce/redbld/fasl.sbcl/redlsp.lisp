(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'REDLSP)) 
(NULL (SETQ *MODE 'SYMBOLIC)) 
(GLOBAL
 '(*LISPARITHEXPOPS* *LISPLOGEXPOPS* *REDARITHEXPOPS* *REDLOGEXPOPS*
   *REDRESWDS* *REDSTMTGPOPS* *REDSTMTOPS*)) 
(SETQ *REDARITHEXPOPS* '(DIFFERENCE EXPT MINUS PLUS QUOTIENT RECIP TIMES)) 
(SETQ *REDLOGEXPOPS* '(AND EQUAL GEQ GREATERP LEQ LESSP NEQ NOT OR)) 
(SETQ *REDRESWDS*
        '(AND RBLOCK COND DE DIFFERENCE END EQUAL EXPT ~FOR FOR GEQ GETEL GO
              GREATERP LEQ LESSP LIST MINUS NEQ NOT OR PLUS PLUS2 PROG PROGN
              PROCEDURE QUOTIENT READ RECIP REPEAT RETURN SETEL SETK SETQ STOP
              TIMES TIMES2 WHILE WRITE)) 
(SETQ *REDSTMTGPOPS* '(RBLOCK PROGN)) 
(SETQ *REDSTMTOPS* '(COND END ~FOR FOR GO REPEAT RETURN SETQ STOP WHILE WRITE)) 
(FLUID '(*PERIOD)) 
(GLOBAL '(DEFTYPE*)) 
(GLOBAL '(*DO* *FOR*)) 
(GLOBAL '(IRENA-CONSTANTS)) 
(SETQ IRENA-CONSTANTS NIL) 
(PUT 'LISPCODE 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODE 'DEFINED-ON-LINE '62) 
(PUT 'LISPCODE 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODE (FORMS)
    (PROG (F FORALL-RESULT FORALL-ENDPTR)
      (SETQ F FORMS)
      (COND ((NULL F) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (F)
                          (COND ((REDEXPP F) (LISPCODEEXP F *PERIOD))
                                ((OR (REDSTMTP F) (REDSTMTGPP F))
                                 (LISPCODESTMT F))
                                ((REDDEFP F) (LISPCODEDEF F))
                                ((PAIRP F)
                                 (PROG (E FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ E F)
                                   (COND ((NULL E) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (E) (LISPCODE E))
                                                     (CAR E))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ E (CDR E))
                                   (COND ((NULL E) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (E) (LISPCODE E)) (CAR E))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))))
                        (CAR F))
                       NIL)))
     LOOPLABEL
      (SETQ F (CDR F))
      (COND ((NULL F) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (F)
                  (COND ((REDEXPP F) (LISPCODEEXP F *PERIOD))
                        ((OR (REDSTMTP F) (REDSTMTGPP F)) (LISPCODESTMT F))
                        ((REDDEFP F) (LISPCODEDEF F))
                        ((PAIRP F)
                         (PROG (E FORALL-RESULT FORALL-ENDPTR)
                           (SETQ E F)
                           (COND ((NULL E) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (E) (LISPCODE E)) (CAR E))
                                            NIL)))
                          LOOPLABEL
                           (SETQ E (CDR E))
                           (COND ((NULL E) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS ((LAMBDA (E) (LISPCODE E)) (CAR E))
                                         NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))))
                (CAR F))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT 'CHECK-FOR-IRENA-CONSTANTS 'NUMBER-OF-ARGS 1) 
(PUT 'CHECK-FOR-IRENA-CONSTANTS 'DEFINED-ON-LINE '73) 
(PUT 'CHECK-FOR-IRENA-CONSTANTS 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'CHECK-FOR-IRENA-CONSTANTS 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE CHECK-FOR-IRENA-CONSTANTS (FORM)
    (COND
     ((AND (LISTP FORM) (MEMQ (CAR FORM) *REDARITHEXPOPS*))
      (PROG (U)
        (SETQ U (CDR FORM))
       LAB
        (COND ((NULL U) (RETURN NIL)))
        ((LAMBDA (U) (CHECK-FOR-IRENA-CONSTANTS U)) (CAR U))
        (SETQ U (CDR U))
        (GO LAB)))
     ((AND (PAIRP FORM) (MEMQ (CAR FORM) '(|:CR:| |:CRN:| |:GI:|)))
      (PROG ()
       REPEATLABEL
        (PROGN
         (SETQ FORM (CDR FORM))
         (CHECK-FOR-IRENA-CONSTANTS (COND ((ATOM FORM) FORM) (T (CAR FORM))))
         NIL)
        (COND ((NOT (ATOM FORM)) (GO REPEATLABEL)))))
     ((AND FORM (ATOM FORM))
      (COND ((MEMQ FORM IRENA-CONSTANTS) (SET (GET FORM '*FOUND-FLAG) T)))))) 
(PUT 'LISPCODEEXP 'NUMBER-OF-ARGS 2) 
(PUT 'LISPCODEEXP 'DEFINED-ON-LINE '86) 
(PUT 'LISPCODEEXP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODEEXP 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE LISPCODEEXP (FORM FP)
    (PROG ()
      (RETURN
       (COND ((NUMBERP FORM) (COND (FP (FLOAT FORM)) (T FORM)))
             ((EQ FORM 'E) (LISPCODEEXP (LIST 'EXP 1.0) FP))
             ((OR (ATOM FORM)
                  (MEMQ (CAR FORM) '(|:RD:| |:CR:| |:CRN:| |:GI:|)))
              (PROGN
               (COND
                ((AND IRENA-CONSTANTS FORM (NOT (STRINGP FORM)))
                 (CHECK-FOR-IRENA-CONSTANTS FORM)))
               FORM))
             ((EQ (CAR FORM) 'EXPT)
              (COND
               ((EQ (CADR FORM) 'E) (LISPCODEEXP (LIST 'EXP (CADDR FORM)) FP))
               ((EQUAL (CADDR FORM) '(QUOTIENT 1 2))
                (LISPCODEEXP (LIST 'SQRT (CADR FORM)) FP))
               ((EQCAR (CADDR FORM) '|:RD:|)
                (PROG (R)
                  (SETQ R (REALRAT (CADDR FORM)))
                  (RETURN
                   (COND
                    ((EQUAL R '(1 . 2))
                     (LIST 'SQRT (LISPCODEEXP (CADR FORM) FP)))
                    (T
                     (LIST 'EXPT (LISPCODEEXP (CADR FORM) FP)
                           (LISPCODEEXP (LIST 'QUOTIENT (CAR R) (CDR R))
                            NIL)))))))
               (T
                (LIST 'EXPT (LISPCODEEXP (CADR FORM) FP)
                      (LISPCODEEXP (CADDR FORM) NIL)))))
             ((EQ (CAR FORM) 'QUOTIENT)
              (LIST 'QUOTIENT (LISPCODEEXP (CADR FORM) T)
                    (LISPCODEEXP (CADDR FORM) T)))
             ((EQ (CAR FORM) 'RECIP)
              (COND (*PERIOD (LIST 'QUOTIENT 1.0 (LISPCODEEXP (CADR FORM) FP)))
                    (T (LIST 'QUOTIENT 1 (LISPCODEEXP (CADR FORM) FP)))))
             ((EQ (CAR FORM) 'DIFFERENCE)
              (LIST 'PLUS (LISPCODEEXP (CADR FORM) FP)
                    (LIST 'MINUS (LISPCODEEXP (CADDR FORM) FP))))
             ((AND (NOT (MEMQ (CAR FORM) *LISPARITHEXPOPS*))
                   (NOT (MEMQ (CAR FORM) *LISPLOGEXPOPS*)))
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT FORM)
                (COND ((NULL ELT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT) (LISPCODEEXP ELT NIL))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ELT) (LISPCODEEXP ELT NIL)) (CAR ELT))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
             (T
              (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
                (SETQ ELT FORM)
                (COND ((NULL ELT) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ELT) (LISPCODEEXP ELT FP))
                                  (CAR ELT))
                                 NIL)))
               LOOPLABEL
                (SETQ ELT (CDR ELT))
                (COND ((NULL ELT) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ELT) (LISPCODEEXP ELT FP)) (CAR ELT))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))))))) 
(PUT 'LISPCODESTMT 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODESTMT 'DEFINED-ON-LINE '152) 
(PUT 'LISPCODESTMT 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODESTMT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODESTMT (FORM)
    (COND ((ATOM FORM) FORM) ((REDASSIGNP FORM) (LISPCODEASSIGN FORM))
          ((REDREADP FORM) (LISPCODEREAD FORM))
          ((REDPRINTP FORM) (LISPCODEPRINT FORM))
          ((REDWHILEP FORM) (LISPCODEWHILE FORM))
          ((REDREPEATP FORM) (LISPCODEREPEAT FORM))
          ((REDFORP FORM) (LISPCODEFOR FORM))
          ((REDCONDP FORM) (LISPCODECOND FORM))
          ((REDRETURNP FORM) (LISPCODERETURN FORM))
          ((REDSTMTGPP FORM) (LISPCODESTMTGP FORM))
          ((REDDEFP FORM) (LISPCODEDEF FORM))
          ((EQ (CAR FORM) 'LITERAL)
           (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
             (SETQ ELT FORM)
             (COND ((NULL ELT) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (ELT) (LISPCODEEXP ELT NIL)) (CAR ELT))
                              NIL)))
            LOOPLABEL
             (SETQ ELT (CDR ELT))
             (COND ((NULL ELT) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (ELT) (LISPCODEEXP ELT NIL)) (CAR ELT))
                           NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL)))
          (T
           (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
             (SETQ ELT FORM)
             (COND ((NULL ELT) (RETURN NIL)))
             (SETQ FORALL-RESULT
                     (SETQ FORALL-ENDPTR
                             (CONS
                              ((LAMBDA (ELT) (LISPCODEEXP ELT *PERIOD))
                               (CAR ELT))
                              NIL)))
            LOOPLABEL
             (SETQ ELT (CDR ELT))
             (COND ((NULL ELT) (RETURN FORALL-RESULT)))
             (RPLACD FORALL-ENDPTR
                     (CONS ((LAMBDA (ELT) (LISPCODEEXP ELT *PERIOD)) (CAR ELT))
                           NIL))
             (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
             (GO LOOPLABEL))))) 
(PUT 'LISPCODEASSIGN 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODEASSIGN 'DEFINED-ON-LINE '181) 
(PUT 'LISPCODEASSIGN 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODEASSIGN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODEASSIGN (FORM)
    (COND
     ((EQCAR (CADDR FORM) 'MAT)
      (PROG (NAME R C RELTS RESULT FTYPE)
        (SETQ NAME (CADR FORM))
        (SETQ FORM (CADDR FORM))
        (SETQ R (SETQ C 1))
        (SETQ FTYPE (SYMTABGET NIL NAME))
        (COND ((NULL FTYPE) (SETQ FTYPE *PERIOD))
              (T
               (PROGN
                (SETQ FTYPE (CADR FTYPE))
                (SETQ FTYPE
                        (COND
                         ((OR (EQUAL FTYPE 'INTEGER)
                              (AND (EQUAL FTYPE 'SCALAR)
                                   (EQUAL DEFTYPE* 'INTEGER)))
                          NIL)
                         (T *PERIOD)))
                NIL)))
        (PROG ()
         WHILELABEL
          (COND ((NOT (SETQ FORM (CDR FORM))) (RETURN NIL)))
          (PROGN
           (SETQ RELTS (CAR FORM))
           (PROG ()
            REPEATLABEL
             (PROGN
              (SETQ RESULT
                      (CONS
                       (MKASSIGN (LIST NAME R C)
                        (LISPCODEEXP (CAR RELTS) FTYPE))
                       RESULT))
              (SETQ C (ADD1 C)))
             (COND ((NOT (NULL (SETQ RELTS (CDR RELTS)))) (GO REPEATLABEL))))
           (SETQ R (ADD1 R))
           (SETQ C 1))
          (GO WHILELABEL))
        (RETURN (MKSTMTGP NIL (REVERSE RESULT)))))
     (T
      (PROG (FTYPE NAME)
        (SETQ NAME (CADR FORM))
        (COND ((PAIRP NAME) (SETQ NAME (CAR NAME))))
        (SETQ FTYPE (SYMTABGET NIL NAME))
        (COND ((NULL FTYPE) (SETQ FTYPE *PERIOD))
              (T
               (PROGN
                (SETQ FTYPE (CADR FTYPE))
                (SETQ FTYPE
                        (COND
                         ((OR (EQUAL FTYPE 'INTEGER)
                              (AND (EQUAL FTYPE 'SCALAR)
                                   (EQUAL DEFTYPE* 'INTEGER)))
                          NIL)
                         (T *PERIOD)))
                NIL)))
        (COND
         ((EQ (CADR FORM) 'E)
          (RETURN (MKASSIGN 'E (LISPCODEEXP (CADDR FORM) FTYPE))))
         (T
          (RETURN
           (MKASSIGN (LISPCODEEXP (CADR FORM) FTYPE)
            (LISPCODEEXP (CADDR FORM) FTYPE))))))))) 
(PUT 'LISPCODEREAD 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODEREAD 'DEFINED-ON-LINE '239) 
(PUT 'LISPCODEREAD 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODEREAD 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODEREAD (FORM) (LIST 'READ (LISPCODEEXP (CADR FORM) NIL))) 
(PUT 'LISPCODEPRINT 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODEPRINT 'DEFINED-ON-LINE '243) 
(PUT 'LISPCODEPRINT 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODEPRINT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODEPRINT (FORM)
    (CONS 'WRITE
          (PROG (ELT FORALL-RESULT FORALL-ENDPTR)
            (SETQ ELT (CDR FORM))
            (COND ((NULL ELT) (RETURN NIL)))
            (SETQ FORALL-RESULT
                    (SETQ FORALL-ENDPTR
                            (CONS
                             ((LAMBDA (ELT) (LISPCODEEXP ELT *PERIOD))
                              (CAR ELT))
                             NIL)))
           LOOPLABEL
            (SETQ ELT (CDR ELT))
            (COND ((NULL ELT) (RETURN FORALL-RESULT)))
            (RPLACD FORALL-ENDPTR
                    (CONS ((LAMBDA (ELT) (LISPCODEEXP ELT *PERIOD)) (CAR ELT))
                          NIL))
            (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
            (GO LOOPLABEL)))) 
(PUT 'LISPCODEWHILE 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODEWHILE 'DEFINED-ON-LINE '246) 
(PUT 'LISPCODEWHILE 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODEWHILE 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODEWHILE (FORM)
    (CONS 'WHILE
          (CONS (LISPCODEEXP (CADR FORM) *PERIOD)
                (PROG (ST FORALL-RESULT FORALL-ENDPTR)
                  (SETQ ST (CDDR FORM))
                  (COND ((NULL ST) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (ST) (LISPCODESTMT ST)) (CAR ST))
                                   NIL)))
                 LOOPLABEL
                  (SETQ ST (CDR ST))
                  (COND ((NULL ST) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (ST) (LISPCODESTMT ST)) (CAR ST))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL))))) 
(PUT 'LISPCODEREPEAT 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODEREPEAT 'DEFINED-ON-LINE '250) 
(PUT 'LISPCODEREPEAT 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODEREPEAT 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODEREPEAT (FORM)
    (PROG (BODY LOGEXP)
      (SETQ BODY (REVERSE (CDR FORM)))
      (SETQ LOGEXP (CAR BODY))
      (SETQ BODY (REVERSE (CDR BODY)))
      (RETURN
       (CONS 'REPEAT
             (APPEND
              (PROG (ST FORALL-RESULT FORALL-ENDPTR)
                (SETQ ST BODY)
                (COND ((NULL ST) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ST) (LISPCODESTMT ST)) (CAR ST))
                                 NIL)))
               LOOPLABEL
                (SETQ ST (CDR ST))
                (COND ((NULL ST) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (ST) (LISPCODESTMT ST)) (CAR ST)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL))
              (LIST (LISPCODEEXP LOGEXP *PERIOD))))))) 
(PUT 'LISPCODEFOR 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODEFOR 'DEFINED-ON-LINE '260) 
(PUT 'LISPCODEFOR 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODEFOR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODEFOR (FORM)
    (COND
     ((EQ (CAR FORM) 'FOR)
      (PROG (EXPLST STMTLST)
        (SETQ EXPLST (LIST (CADR FORM) (CADDR FORM)))
        (SETQ STMTLST (CDDDDR FORM))
        (RETURN
         (APPEND
          (CONS *FOR*
                (PROG (EXP FORALL-RESULT FORALL-ENDPTR)
                  (SETQ EXP EXPLST)
                  (COND ((NULL EXP) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (EXP) (LISPCODEEXP EXP NIL))
                                    (CAR EXP))
                                   NIL)))
                 LOOPLABEL
                  (SETQ EXP (CDR EXP))
                  (COND ((NULL EXP) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS
                           ((LAMBDA (EXP) (LISPCODEEXP EXP NIL)) (CAR EXP))
                           NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
          (CONS *DO*
                (PROG (ST FORALL-RESULT FORALL-ENDPTR)
                  (SETQ ST STMTLST)
                  (COND ((NULL ST) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS
                                   ((LAMBDA (ST) (LISPCODESTMT ST)) (CAR ST))
                                   NIL)))
                 LOOPLABEL
                  (SETQ ST (CDR ST))
                  (COND ((NULL ST) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (ST) (LISPCODESTMT ST)) (CAR ST))
                                NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))))))
     (T
      (PROG (VAR1 VAR EXPLST OP EXP)
        (SETQ VAR1 (CADR FORM))
        (SETQ FORM (CADDR FORM))
        (SETQ VAR (CADR FORM))
        (SETQ EXPLST (CADDR FORM))
        (COND ((EQ (CADDDR FORM) 'SUM) (SETQ OP 'PLUS)) (T (SETQ OP 'TIMES)))
        (SETQ EXP (CAR (CDDDDR FORM)))
        (SETQ FORM
                (LIST 'PROG NIL
                      (LISPCODE
                       (LIST 'SETQ VAR1 (COND ((EQ OP 'PLUS) 0) (T 1))))
                      (LISPCODE
                       (LIST *FOR* VAR EXPLST *DO*
                             (LIST 'SETQ VAR1 (LIST OP VAR1 EXP))))))
        (RETURN (LISPCODESTMT FORM)))))) 
(PUT 'LISPCODECOND 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODECOND 'DEFINED-ON-LINE '296) 
(PUT 'LISPCODECOND 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODECOND 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODECOND (FORM)
    (PROG (RESULT PR)
      (PROG ()
       WHILELABEL
        (COND ((NOT (SETQ FORM (CDR FORM))) (RETURN NIL)))
        (PROGN
         (SETQ PR (CAR FORM))
         (SETQ PR
                 (CONS (LISPCODEEXP (CAR PR) *PERIOD)
                       (PROG (STMT FORALL-RESULT FORALL-ENDPTR)
                         (SETQ STMT (CDR PR))
                         (COND ((NULL STMT) (RETURN NIL)))
                         (SETQ FORALL-RESULT
                                 (SETQ FORALL-ENDPTR
                                         (CONS
                                          ((LAMBDA (STMT) (LISPCODESTMT STMT))
                                           (CAR STMT))
                                          NIL)))
                        LOOPLABEL
                         (SETQ STMT (CDR STMT))
                         (COND ((NULL STMT) (RETURN FORALL-RESULT)))
                         (RPLACD FORALL-ENDPTR
                                 (CONS
                                  ((LAMBDA (STMT) (LISPCODESTMT STMT))
                                   (CAR STMT))
                                  NIL))
                         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                         (GO LOOPLABEL))))
         (SETQ RESULT (CONS PR RESULT)))
        (GO WHILELABEL))
      (RETURN (MKCOND (REVERSE RESULT))))) 
(PUT 'LISPCODERETURN 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODERETURN 'DEFINED-ON-LINE '309) 
(PUT 'LISPCODERETURN 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODERETURN 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODERETURN (FORM)
    (COND ((MEMBER FORM '((RETURN) (RETURN NIL))) (LIST 'RETURN))
          (T (MKRETURN (LISPCODEEXP (CADR FORM) *PERIOD))))) 
(PUT 'LISPCODESTMTGP 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODESTMTGP 'DEFINED-ON-LINE '316) 
(PUT 'LISPCODESTMTGP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODESTMTGP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODESTMTGP (FORM)
    (COND
     ((MEMQ (CAR FORM) '(PROG RBLOCK))
      (MKSTMTGP (CADR FORM)
       (PROG (STMT FORALL-RESULT FORALL-ENDPTR)
         (SETQ STMT (CDDR FORM))
         (COND ((NULL STMT) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (STMT) (LISPCODESTMT STMT)) (CAR STMT))
                               NIL)))
        LOOPLABEL
         (SETQ STMT (CDR STMT))
         (COND ((NULL STMT) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (STMT) (LISPCODESTMT STMT)) (CAR STMT)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))
     (T
      (MKSTMTGP 0
       (PROG (STMT FORALL-RESULT FORALL-ENDPTR)
         (SETQ STMT (CDR FORM))
         (COND ((NULL STMT) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (STMT) (LISPCODESTMT STMT)) (CAR STMT))
                               NIL)))
        LOOPLABEL
         (SETQ STMT (CDR STMT))
         (COND ((NULL STMT) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (STMT) (LISPCODESTMT STMT)) (CAR STMT)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))))) 
(PUT 'LISPCODEDEF 'NUMBER-OF-ARGS 1) 
(PUT 'LISPCODEDEF 'DEFINED-ON-LINE '325) 
(PUT 'LISPCODEDEF 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'LISPCODEDEF 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE LISPCODEDEF (FORM)
    (COND
     ((EQ (CAR FORM) 'PROCEDURE)
      (MKDEF (CADR FORM) (CAR (CDDDDR FORM))
       (PROG (STMT FORALL-RESULT FORALL-ENDPTR)
         (SETQ STMT (CDR (CDDDDR FORM)))
         (COND ((NULL STMT) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (STMT) (LISPCODESTMT STMT)) (CAR STMT))
                               NIL)))
        LOOPLABEL
         (SETQ STMT (CDR STMT))
         (COND ((NULL STMT) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (STMT) (LISPCODESTMT STMT)) (CAR STMT)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL))))
     (T
      (MKDEF (CADR FORM) (CADDR FORM)
       (PROG (STMT FORALL-RESULT FORALL-ENDPTR)
         (SETQ STMT (CDDDR FORM))
         (COND ((NULL STMT) (RETURN NIL)))
         (SETQ FORALL-RESULT
                 (SETQ FORALL-ENDPTR
                         (CONS ((LAMBDA (STMT) (LISPCODESTMT STMT)) (CAR STMT))
                               NIL)))
        LOOPLABEL
         (SETQ STMT (CDR STMT))
         (COND ((NULL STMT) (RETURN FORALL-RESULT)))
         (RPLACD FORALL-ENDPTR
                 (CONS ((LAMBDA (STMT) (LISPCODESTMT STMT)) (CAR STMT)) NIL))
         (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
         (GO LOOPLABEL)))))) 
(PUT 'REDASSIGNP 'NUMBER-OF-ARGS 1) 
(PUT 'REDASSIGNP 'DEFINED-ON-LINE '339) 
(PUT 'REDASSIGNP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDASSIGNP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDASSIGNP (FORM) (AND (EQCAR FORM 'SETQ) (REDASSIGN1P (CADDR FORM)))) 
(PUT 'REDASSIGN1P 'NUMBER-OF-ARGS 1) 
(PUT 'REDASSIGN1P 'DEFINED-ON-LINE '342) 
(PUT 'REDASSIGN1P 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDASSIGN1P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDASSIGN1P (FORM)
    (COND ((ATOM FORM) T) ((EQ (CAR FORM) 'SETQ) (REDASSIGN1P (CADDR FORM)))
          ((MEMQ (CAR FORM) '(READ FOR)) NIL) (T T))) 
(PUT 'REDCONDP 'NUMBER-OF-ARGS 1) 
(PUT 'REDCONDP 'DEFINED-ON-LINE '352) 
(PUT 'REDCONDP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDCONDP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDCONDP (FORM) (EQCAR FORM 'COND)) 
(PUT 'REDDEFP 'NUMBER-OF-ARGS 1) 
(PUT 'REDDEFP 'DEFINED-ON-LINE '355) 
(PUT 'REDDEFP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDDEFP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDDEFP (FORM) (EQCAR FORM 'PROCEDURE)) 
(PUT 'REDEXPP 'NUMBER-OF-ARGS 1) 
(PUT 'REDEXPP 'DEFINED-ON-LINE '358) 
(PUT 'REDEXPP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDEXPP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDEXPP (FORM)
    (OR (ATOM FORM) (MEMQ (CAR FORM) *REDARITHEXPOPS*)
        (MEMQ (CAR FORM) *REDLOGEXPOPS*) (NOT (MEMQ (CAR FORM) *REDRESWDS*)))) 
(PUT 'REDFORP 'NUMBER-OF-ARGS 1) 
(PUT 'REDFORP 'DEFINED-ON-LINE '364) 
(PUT 'REDFORP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDFORP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFORP (FORM)
    (COND
     ((PAIRP FORM)
      (COND ((EQ (CAR FORM) 'FOR) T)
            ((EQ (CAR FORM) 'SETQ) (REDFOR1P (CADDR FORM))))))) 
(PUT 'REDFOR1P 'NUMBER-OF-ARGS 1) 
(PUT 'REDFOR1P 'DEFINED-ON-LINE '371) 
(PUT 'REDFOR1P 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDFOR1P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDFOR1P (FORM)
    (COND ((ATOM FORM) NIL) ((EQ (CAR FORM) 'SETQ) (REDFOR1P (CADDR FORM)))
          ((EQ (CAR FORM) 'FOR) T))) 
(PUT 'REDPRINTP 'NUMBER-OF-ARGS 1) 
(PUT 'REDPRINTP 'DEFINED-ON-LINE '379) 
(PUT 'REDPRINTP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDPRINTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDPRINTP (FORM) (EQCAR FORM 'WRITE)) 
(PUT 'REDREADP 'NUMBER-OF-ARGS 1) 
(PUT 'REDREADP 'DEFINED-ON-LINE '382) 
(PUT 'REDREADP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDREADP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDREADP (FORM) (AND (EQCAR FORM 'SETQ) (REDREAD1P (CADDR FORM)))) 
(PUT 'REDREAD1P 'NUMBER-OF-ARGS 1) 
(PUT 'REDREAD1P 'DEFINED-ON-LINE '385) 
(PUT 'REDREAD1P 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDREAD1P 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDREAD1P (FORM)
    (COND ((ATOM FORM) NIL) ((EQ (CAR FORM) 'SETQ) (REDREAD1P (CADDR FORM)))
          ((EQ (CAR FORM) 'READ) T))) 
(PUT 'REDREPEATP 'NUMBER-OF-ARGS 1) 
(PUT 'REDREPEATP 'DEFINED-ON-LINE '393) 
(PUT 'REDREPEATP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDREPEATP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDREPEATP (FORM) (EQCAR FORM 'REPEAT)) 
(PUT 'REDRETURNP 'NUMBER-OF-ARGS 1) 
(PUT 'REDRETURNP 'DEFINED-ON-LINE '396) 
(PUT 'REDRETURNP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDRETURNP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDRETURNP (FORM) (EQCAR FORM 'RETURN)) 
(PUT 'REDSTMTP 'NUMBER-OF-ARGS 1) 
(PUT 'REDSTMTP 'DEFINED-ON-LINE '399) 
(PUT 'REDSTMTP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDSTMTP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDSTMTP (FORM)
    (OR (ATOM FORM) (MEMQ (CAR FORM) *REDSTMTOPS*)
        (AND (ATOM (CAR FORM)) (NOT (MEMQ (CAR FORM) *REDRESWDS*))))) 
(PUT 'REDSTMTGPP 'NUMBER-OF-ARGS 1) 
(PUT 'REDSTMTGPP 'DEFINED-ON-LINE '404) 
(PUT 'REDSTMTGPP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDSTMTGPP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDSTMTGPP (FORM) (AND (PAIRP FORM) (MEMQ (CAR FORM) *REDSTMTGPOPS*))) 
(PUT 'REDWHILEP 'NUMBER-OF-ARGS 1) 
(PUT 'REDWHILEP 'DEFINED-ON-LINE '407) 
(PUT 'REDWHILEP 'DEFINED-IN-FILE 'GENTRAN/REDLSP.RED) 
(PUT 'REDWHILEP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE REDWHILEP (FORM) (EQCAR FORM 'WHILE)) 
(ENDMODULE) 