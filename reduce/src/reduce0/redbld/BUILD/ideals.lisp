(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'IDEALS)) 
(CREATE-PACKAGE '(IDEALS) '(CONTRIB GROEBNER)) 
(IMPORTS (LIST 'GROEBNER)) 
(LOAD-PACKAGE 'GROEBNER) 
(FLUID '(GB-LIST*)) 
(GLOBAL '(ID-VARS*)) 
(SHARE (LIST 'ID-VARS*)) 
(IMPORTS (LIST 'IDQUOTIENTEVAL 'GROEBNEREVAL 'PREDUCEEVAL 'TORDER)) 
(EXPORTS
 (LIST 'GB 'GB-EQUAL 'GB-ITERSECT 'GB-MEMBER 'GB-QUOTIENT 'GB-PLUS 'GB-SUBSET
       'GB-TIMES 'I-SETTING 'IDEALP 'IDEAL2LIST 'ID-EQUAL 'ID-QUOTIENT
       'INTERSECTION 'MEMBER 'OVER 'SUBSET)) 
(PUT 'I-SETTING 'NUMBER-OF-ARGS 1) 
(PUT 'I-SETTING 'DEFINED-ON-LINE '51) 
(PUT 'I-SETTING 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'I-SETTING 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE I-SETTING (U)
    (PROG (O)
      (SETQ O ID-VARS*)
      (SETQ ID-VARS*
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST
                     (PROG (X FORALL-RESULT FORALL-ENDPTR)
                       (SETQ X U)
                       (COND ((NULL X) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (X) (REVAL1 X T)) (CAR X))
                                        NIL)))
                      LOOPLABEL
                       (SETQ X (CDR X))
                       (COND ((NULL X) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (X) (REVAL1 X T)) (CAR X)) NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ GB-LIST* NIL)
      (RETURN O))) 
(PUT 'I_SETTING 'PSOPFN 'I-SETTING) 
(AEVAL (OPERATOR (LIST 'I))) 
(PUT 'IDEAL2LIST 'NUMBER-OF-ARGS 1) 
(PUT 'IDEAL2LIST 'DEFINED-ON-LINE '61) 
(PUT 'IDEAL2LIST 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'IDEAL2LIST 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDEAL2LIST (U) (CONS 'LIST (CDR (TEST-IDEAL U)))) 
(FLAG '(IDEAL2LIST) 'OPFN) 
(PUT 'GB 'NUMBER-OF-ARGS 1) 
(PUT 'GB 'DEFINED-ON-LINE '65) 
(PUT 'GB 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GB (U)
    (PROG (V W)
      (SETQ U (TEST-IDEAL (REVAL1 U T)))
      (SETQ V (LIST U ID-VARS* VDPSORTMODE*))
      (SETQ W (ASSOC V GB-LIST*))
      (RETURN (COND (W (CDR W)) (T (GB-NEW U)))))) 
(PUT 'GB-NEW 'NUMBER-OF-ARGS 1) 
(PUT 'GB-NEW 'DEFINED-ON-LINE '72) 
(PUT 'GB-NEW 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-NEW 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GB-NEW (U)
    (PROG (V W)
      (SETQ U (TEST-IDEAL (REVAL1 U T)))
      (SETQ V (LIST U ID-VARS* VDPSORTMODE*))
      (SETQ W
              (CONS 'I
                    (CDR (GROEBNEREVAL (LIST (CONS 'LIST (CDR U)) ID-VARS*)))))
      (SETQ GB-LIST* (CONS (CONS V W) GB-LIST*))
      (SETQ GB-LIST* (CONS (CONS (CONS W (CDR V)) W) GB-LIST*))
      (RETURN W))) 
(FLAG '(GB) 'OPFN) 
(PUT 'TEST-IDEAL 'NUMBER-OF-ARGS 1) 
(PUT 'TEST-IDEAL 'DEFINED-ON-LINE '82) 
(PUT 'TEST-IDEAL 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'TEST-IDEAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE TEST-IDEAL (U)
    (COND
     ((NOT (EQCAR ID-VARS* 'LIST))
      (TYPERR ID-VARS* "ideal setting; set variables first"))
     ((EQCAR U 'LIST) (CONS 'I (CDR U)))
     ((NOT (EQCAR U 'I)) (TYPERR U "polynomial ideal")) (T U))) 
(PUT 'IDEALP 'NUMBER-OF-ARGS 1) 
(PUT 'IDEALP 'DEFINED-ON-LINE '88) 
(PUT 'IDEALP 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'IDEALP 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE IDEALP (U) (OR (EQCAR U 'I) (EQCAR U 'LIST))) 
(FLAG '(IDEALP) 'OPFN) 
(NEWTOK '((|.| =) ID-EQUAL)) 
(AEVAL (OPERATOR (LIST 'ID-EQUAL))) 
(INFIX (LIST 'ID-EQUAL)) 
(PRECEDENCE (LIST 'ID-EQUAL 'EQUAL)) 
(PUT 'GB-EQUAL 'NUMBER-OF-ARGS 2) 
(PUT 'GB-EQUAL 'DEFINED-ON-LINE '97) 
(PUT 'GB-EQUAL 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-EQUAL 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GB-EQUAL (A B) (COND ((EQUAL (GB A) (GB B)) 1) (T 0))) 
(FLAG '(GB-EQUAL) 'OPFN) 
(AEVAL
 (LET
  '((REPLACEBY (ID-EQUAL (~ A) (~ B))
     (WHEN (GB-EQUAL A B) (AND (IDEALP A) (IDEALP B))))))) 
(PUT 'GB-MEMBER 'NUMBER-OF-ARGS 2) 
(PUT 'GB-MEMBER 'DEFINED-ON-LINE '103) 
(PUT 'GB-MEMBER 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-MEMBER 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GB-MEMBER (P U)
    (COND ((EQUAL 0 (PREDUCEEVAL (LIST P (IDEAL2LIST (GB U)) ID-VARS*))) 1)
          (T 0))) 
(FLAG '(GB-MEMBER) 'OPFN) 
(AEVAL (OPERATOR (LIST 'MEMBER))) 
(AEVAL
 (LET '((REPLACEBY (MEMBER (~ A) (~ B)) (WHEN (GB-MEMBER A B) (IDEALP B)))))) 
(PUT 'GB-SUBSET 'NUMBER-OF-ARGS 2) 
(PUT 'GB-SUBSET 'DEFINED-ON-LINE '112) 
(PUT 'GB-SUBSET 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-SUBSET 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GB-SUBSET (A B)
    (PROG (Q)
      (SETQ Q T)
      (SETQ A (CDR (TEST-IDEAL (REVAL1 A T))))
      (SETQ B (IDEAL2LIST (GB B)))
      (PROG (P)
        (SETQ P A)
       LAB
        (COND ((NULL P) (RETURN NIL)))
        ((LAMBDA (P)
           (SETQ Q (AND Q (EQUAL 0 (PREDUCEEVAL (LIST P B ID-VARS*))))))
         (CAR P))
        (SETQ P (CDR P))
        (GO LAB))
      (RETURN (COND (Q 1) (T 0))))) 
(FLAG '(GB-SUBSET) 'OPFN) 
(AEVAL (OPERATOR (LIST 'SUBSET))) 
(INFIX (LIST 'SUBSET)) 
(PRECEDENCE (LIST 'SUBSET 'MEMBER)) 
(AEVAL
 (LET
  '((REPLACEBY (SUBSET (~ A) (~ B))
     (WHEN (GB-SUBSET A B) (AND (IDEALP A) (IDEALP B))))))) 
(PUT 'GB-PLUS 'NUMBER-OF-ARGS 2) 
(PUT 'GB-PLUS 'DEFINED-ON-LINE '127) 
(PUT 'GB-PLUS 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-PLUS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GB-PLUS (A B)
    (PROGN
     (SETQ A (CDR (TEST-IDEAL (REVAL1 A T))))
     (SETQ B (CDR (TEST-IDEAL (REVAL1 B T))))
     (GB (CONS 'I (APPEND A B))))) 
(FLAG '(GB-PLUS) 'OPFN) 
(AEVAL (OPERATOR (LIST 'ADD))) 
(AEVAL
 (LET
  '((REPLACEBY (ADD (~ A) (~ B))
     (WHEN (GB-PLUS A B) (AND (IDEALP A) (IDEALP B))))))) 
(PUT 'GB-TIMES 'NUMBER-OF-ARGS 2) 
(PUT 'GB-TIMES 'DEFINED-ON-LINE '137) 
(PUT 'GB-TIMES 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-TIMES 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GB-TIMES (A B)
    (PROGN
     (SETQ A (CDR (TEST-IDEAL (REVAL1 A T))))
     (SETQ B (CDR (TEST-IDEAL (REVAL1 B T))))
     (GB
      (CONS 'I
            (PROG (P FORALL-RESULT FORALL-ENDPTR)
              (SETQ P A)
             STARTOVER
              (COND ((NULL P) (RETURN NIL)))
              (SETQ FORALL-RESULT
                      ((LAMBDA (P)
                         (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                           (SETQ Q B)
                           (COND ((NULL Q) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (Q) (LIST 'TIMES P Q))
                                             (CAR Q))
                                            NIL)))
                          LOOPLABEL
                           (SETQ Q (CDR Q))
                           (COND ((NULL Q) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (Q) (LIST 'TIMES P Q)) (CAR Q))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                       (CAR P)))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
              (SETQ P (CDR P))
              (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
             LOOPLABEL
              (COND ((NULL P) (RETURN FORALL-RESULT)))
              (RPLACD FORALL-ENDPTR
                      ((LAMBDA (P)
                         (PROG (Q FORALL-RESULT FORALL-ENDPTR)
                           (SETQ Q B)
                           (COND ((NULL Q) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (Q) (LIST 'TIMES P Q))
                                             (CAR Q))
                                            NIL)))
                          LOOPLABEL
                           (SETQ Q (CDR Q))
                           (COND ((NULL Q) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (Q) (LIST 'TIMES P Q)) (CAR Q))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL)))
                       (CAR P)))
              (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
              (SETQ P (CDR P))
              (GO LOOPLABEL)))))) 
(FLAG '(GB-TIMES) 'OPFN) 
(AEVAL (OPERATOR (LIST 'MULT))) 
(AEVAL
 (LET
  '((REPLACEBY (MULT (~ A) (~ B))
     (WHEN (GB-TIMES A B) (AND (IDEALP A) (IDEALP B))))))) 
(PUT 'GB-INTERSECT 'NUMBER-OF-ARGS 2) 
(PUT 'GB-INTERSECT 'DEFINED-ON-LINE '147) 
(PUT 'GB-INTERSECT 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-INTERSECT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GB-INTERSECT (A B)
    (PROG (TT OO Q V)
      (SETQ TT '--T)
      (SETQ V ID-VARS*)
      (SETQ OO (EVAL '(TORDER '(LEX))))
      (SETQ A (CDR (TEST-IDEAL (REVAL1 A T))))
      (SETQ B (CDR (TEST-IDEAL (REVAL1 B T))))
      (SETQ Q
              (CONS 'I
                    (APPEND
                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                       (SETQ P A)
                       (COND ((NULL P) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (P) (LIST 'TIMES TT P))
                                         (CAR P))
                                        NIL)))
                      LOOPLABEL
                       (SETQ P (CDR P))
                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS ((LAMBDA (P) (LIST 'TIMES TT P)) (CAR P))
                                     NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL))
                     (PROG (P FORALL-RESULT FORALL-ENDPTR)
                       (SETQ P B)
                       (COND ((NULL P) (RETURN NIL)))
                       (SETQ FORALL-RESULT
                               (SETQ FORALL-ENDPTR
                                       (CONS
                                        ((LAMBDA (P)
                                           (LIST 'TIMES (LIST 'DIFFERENCE 1 TT)
                                                 P))
                                         (CAR P))
                                        NIL)))
                      LOOPLABEL
                       (SETQ P (CDR P))
                       (COND ((NULL P) (RETURN FORALL-RESULT)))
                       (RPLACD FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (P)
                                   (LIST 'TIMES (LIST 'DIFFERENCE 1 TT) P))
                                 (CAR P))
                                NIL))
                       (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                       (GO LOOPLABEL)))))
      (SETQ ID-VARS*
              (PROGN
               (SETQ ALGLIST* (CONS NIL NIL))
               (CONS 'LIST (CONS TT (CDR ID-VARS*)))))
      (SETQ Q (ERRORSET (LIST 'GB (MKQUOTE Q)) NIL *BACKTRACE))
      (SETQ ID-VARS* (PROGN (SETQ ALGLIST* (CONS NIL NIL)) V))
      (EVAL (LIST 'TORDER (MKQUOTE (LIST OO))))
      (COND ((ERRORP Q) (REDERR "ideal intersection failed")))
      (SETQ Q
              (PROG (P FORALL-RESULT FORALL-ENDPTR)
                (SETQ P (CDAR Q))
               STARTOVER
                (COND ((NULL P) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        ((LAMBDA (P) (COND ((NOT (SMEMQ TT P)) (LIST P))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                (SETQ P (CDR P))
                (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
               LOOPLABEL
                (COND ((NULL P) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        ((LAMBDA (P) (COND ((NOT (SMEMQ TT P)) (LIST P))))
                         (CAR P)))
                (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                (SETQ P (CDR P))
                (GO LOOPLABEL)))
      (RETURN (GB (CONS 'I Q))))) 
(FLAG '(GB-INTERSECT) 'OPFN) 
(AEVAL (OPERATOR (LIST 'INTERSECTION))) 
(AEVAL
 (LET
  '((REPLACEBY (INTERSECTION (~ A) (~ B))
     (WHEN (GB-INTERSECT A B) (AND (IDEALP A) (IDEALP B))))))) 
(NEWTOK '((|.| |:|) ID-QUOTIENT)) 
(AEVAL (OPERATOR (LIST 'ID-QUOTIENT))) 
(INFIX (LIST 'ID-QUOTIENT)) 
(PRECEDENCE (LIST 'ID-QUOTIENT 'QUOTIENT)) 
(PUT 'GB-QUOTIENT 'NUMBER-OF-ARGS 2) 
(PUT 'GB-QUOTIENT 'DEFINED-ON-LINE '176) 
(PUT 'GB-QUOTIENT 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-QUOTIENT 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GB-QUOTIENT (A B)
    (PROGN
     (SETQ A (TEST-IDEAL (REVAL1 A T)))
     (SETQ B (TEST-IDEAL (REVAL1 B T)))
     (GB-QUOTIENT1 A (CDR B)))) 
(PUT 'GB-QUOTIENT1 'NUMBER-OF-ARGS 2) 
(PUT 'GB-QUOTIENT1 'DEFINED-ON-LINE '179) 
(PUT 'GB-QUOTIENT1 'DEFINED-IN-FILE 'GROEBNER/IDEALS.RED) 
(PUT 'GB-QUOTIENT1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GB-QUOTIENT1 (A B)
    (PROG (Q)
      (SETQ Q
              (CONS 'I
                    (CDR
                     (IDQUOTIENTEVAL (LIST (IDEAL2LIST A) (CAR B) ID-VARS*)))))
      (RETURN
       (COND ((NULL (CDR B)) Q)
             (T (GB-INTERSECT Q (GB-QUOTIENT1 A (CDR B)))))))) 
(FLAG '(GB-QUOTIENT) 'OPFN) 
(AEVAL (OPERATOR (LIST 'OVER))) 
(AEVAL
 (LET
  '((REPLACEBY (OVER (~ A) (~ B))
     (WHEN (GB-QUOTIENT A B) (AND (IDEALP A) (IDEALP B))))))) 
(AEVAL
 (LET
  '((REPLACEBY (ID-QUOTIENT (~ A) (~ B))
     (WHEN (GB-QUOTIENT A B) (AND (IDEALP A) (IDEALP B))))))) 
(ENDMODULE) 