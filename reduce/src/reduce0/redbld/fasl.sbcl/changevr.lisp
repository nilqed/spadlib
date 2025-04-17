(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'CHANGEVR)) 
(CREATE-PACKAGE '(CHANGEVR) '(CONTRIB MISC)) 
(LOAD-PACKAGE 'MATRIX) 
(FLUID '(POWLIS* WTL*)) 
(FLUID '(*DISPJACOBIAN)) 
(SWITCH (LIST 'DISPJACOBIAN)) 
(PUT 'SIMPCHANGEVAR 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPCHANGEVAR 'DEFINED-ON-LINE '66) 
(PUT 'SIMPCHANGEVAR 'DEFINED-IN-FILE 'MISC/CHANGEVR.RED) 
(PUT 'SIMPCHANGEVAR 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPCHANGEVAR (V)
    (PROG (U F J DVAR FLG RULELIST)
      (SETQ DVAR (COND ((PAIRP (CAR V)) (CDAR V)) (T (CONS (CAR V) NIL))))
      (SETQ V (CDR V))
      (SETQ U (COND ((PAIRP (CAR V)) (CDAR V)) (T (CONS (CAR V) NIL))))
      (SETQ V (CDR V))
      (COND ((EQCAR (CAR V) 'LIST) (SETQ V (APPEND (CDAR V) (CDR V)))))
      (PROG ()
       WHILELABEL
        (COND ((NOT (CDR V)) (RETURN NIL)))
        (PROGN
         (COND
          ((NOT (EQCAR (CAR V) 'EQUAL))
           (REDERR "improper new variable declaration")))
         (SETQ F (CONS (CDAR V) F))
         (SETQ V (CDR V)))
        (GO WHILELABEL))
      (SETQ V (REVAL1 (CAR V) T))
      (COND ((LESSP (LENGTH U) (LENGTH F)) (REDERR "Too few new variables"))
            ((GREATERP (LENGTH U) (LENGTH F))
             (REDERR "Too few old variables")))
      (SETQ J
              (PROG (ENTRY FORALL-RESULT FORALL-ENDPTR)
                (SETQ ENTRY F)
                (COND ((NULL ENTRY) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (ENTRY)
                                    (PROG (NEWVRB FORALL-RESULT FORALL-ENDPTR)
                                      (SETQ NEWVRB U)
                                      (COND ((NULL NEWVRB) (RETURN NIL)))
                                      (SETQ FORALL-RESULT
                                              (SETQ FORALL-ENDPTR
                                                      (CONS
                                                       ((LAMBDA (NEWVRB)
                                                          (REVAL1
                                                           (LIST 'DF
                                                                 (CADR ENTRY)
                                                                 NEWVRB)
                                                           T))
                                                        (CAR NEWVRB))
                                                       NIL)))
                                     LOOPLABEL
                                      (SETQ NEWVRB (CDR NEWVRB))
                                      (COND
                                       ((NULL NEWVRB) (RETURN FORALL-RESULT)))
                                      (RPLACD FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (NEWVRB)
                                                  (REVAL1
                                                   (LIST 'DF (CADR ENTRY)
                                                         NEWVRB)
                                                   T))
                                                (CAR NEWVRB))
                                               NIL))
                                      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                      (GO LOOPLABEL)))
                                  (CAR ENTRY))
                                 NIL)))
               LOOPLABEL
                (SETQ ENTRY (CDR ENTRY))
                (COND ((NULL ENTRY) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (ENTRY)
                            (PROG (NEWVRB FORALL-RESULT FORALL-ENDPTR)
                              (SETQ NEWVRB U)
                              (COND ((NULL NEWVRB) (RETURN NIL)))
                              (SETQ FORALL-RESULT
                                      (SETQ FORALL-ENDPTR
                                              (CONS
                                               ((LAMBDA (NEWVRB)
                                                  (REVAL1
                                                   (LIST 'DF (CADR ENTRY)
                                                         NEWVRB)
                                                   T))
                                                (CAR NEWVRB))
                                               NIL)))
                             LOOPLABEL
                              (SETQ NEWVRB (CDR NEWVRB))
                              (COND ((NULL NEWVRB) (RETURN FORALL-RESULT)))
                              (RPLACD FORALL-ENDPTR
                                      (CONS
                                       ((LAMBDA (NEWVRB)
                                          (REVAL1
                                           (LIST 'DF (CADR ENTRY) NEWVRB) T))
                                        (CAR NEWVRB))
                                       NIL))
                              (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                              (GO LOOPLABEL)))
                          (CAR ENTRY))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ J (CDR (REVAL1 (LIST 'QUOTIENT 1 (CONS 'MAT J)) NIL)))
      (PROG (NEW)
        (SETQ NEW U)
       LAB
        (COND ((NULL NEW) (RETURN NIL)))
        ((LAMBDA (NEW)
           (PROG (OLD)
             (SETQ OLD F)
            LAB
             (COND ((NULL OLD) (RETURN NIL)))
             ((LAMBDA (OLD) (DEPEND1 NEW (CAR OLD) T)) (CAR OLD))
             (SETQ OLD (CDR OLD))
             (GO LAB)))
         (CAR NEW))
        (SETQ NEW (CDR NEW))
        (GO LAB))
      (SETQ RULELIST
              (CONS 'LIST
                    (PROG (ROW FORALL-RESULT FORALL-ENDPTR)
                      (SETQ ROW (PAIR U J))
                     STARTOVER
                      (COND ((NULL ROW) (RETURN NIL)))
                      (SETQ FORALL-RESULT
                              ((LAMBDA (ROW)
                                 (PROG (COL FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ COL (PAIR F (CDR ROW)))
                                   (COND ((NULL COL) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (COL)
                                                       (PROG (RULE)
                                                         (SETQ RULE
                                                                 (LIST
                                                                  'REPLACEBY
                                                                  (LIST 'DF
                                                                        (CAR
                                                                         ROW)
                                                                        (CAAR
                                                                         COL))
                                                                  (SQCHK
                                                                   (CDR COL))))
                                                         (COND
                                                          ((AND *DISPJACOBIAN
                                                                *MSG)
                                                           (MATHPRINT RULE)))
                                                         (RETURN RULE)))
                                                     (CAR COL))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ COL (CDR COL))
                                   (COND ((NULL COL) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (COL)
                                               (PROG (RULE)
                                                 (SETQ RULE
                                                         (LIST 'REPLACEBY
                                                               (LIST 'DF
                                                                     (CAR ROW)
                                                                     (CAAR
                                                                      COL))
                                                               (SQCHK
                                                                (CDR COL))))
                                                 (COND
                                                  ((AND *DISPJACOBIAN *MSG)
                                                   (MATHPRINT RULE)))
                                                 (RETURN RULE)))
                                             (CAR COL))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR ROW)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-RESULT))
                      (SETQ ROW (CDR ROW))
                      (COND ((ATOM FORALL-ENDPTR) (GO STARTOVER)))
                     LOOPLABEL
                      (COND ((NULL ROW) (RETURN FORALL-RESULT)))
                      (RPLACD FORALL-ENDPTR
                              ((LAMBDA (ROW)
                                 (PROG (COL FORALL-RESULT FORALL-ENDPTR)
                                   (SETQ COL (PAIR F (CDR ROW)))
                                   (COND ((NULL COL) (RETURN NIL)))
                                   (SETQ FORALL-RESULT
                                           (SETQ FORALL-ENDPTR
                                                   (CONS
                                                    ((LAMBDA (COL)
                                                       (PROG (RULE)
                                                         (SETQ RULE
                                                                 (LIST
                                                                  'REPLACEBY
                                                                  (LIST 'DF
                                                                        (CAR
                                                                         ROW)
                                                                        (CAAR
                                                                         COL))
                                                                  (SQCHK
                                                                   (CDR COL))))
                                                         (COND
                                                          ((AND *DISPJACOBIAN
                                                                *MSG)
                                                           (MATHPRINT RULE)))
                                                         (RETURN RULE)))
                                                     (CAR COL))
                                                    NIL)))
                                  LOOPLABEL
                                   (SETQ COL (CDR COL))
                                   (COND ((NULL COL) (RETURN FORALL-RESULT)))
                                   (RPLACD FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (COL)
                                               (PROG (RULE)
                                                 (SETQ RULE
                                                         (LIST 'REPLACEBY
                                                               (LIST 'DF
                                                                     (CAR ROW)
                                                                     (CAAR
                                                                      COL))
                                                               (SQCHK
                                                                (CDR COL))))
                                                 (COND
                                                  ((AND *DISPJACOBIAN *MSG)
                                                   (MATHPRINT RULE)))
                                                 (RETURN RULE)))
                                             (CAR COL))
                                            NIL))
                                   (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                   (GO LOOPLABEL)))
                               (CAR ROW)))
                      (SETQ FORALL-ENDPTR (LASTPAIR FORALL-ENDPTR))
                      (SETQ ROW (CDR ROW))
                      (GO LOOPLABEL))))
      (SETQ V (CHANGEARG DVAR U V))
      (PROG (ENTRY)
        (SETQ ENTRY F)
       LAB
        (COND ((NULL ENTRY) (RETURN NIL)))
        ((LAMBDA (ENTRY) (SETQ V (SUBCARE (CAR ENTRY) (CADR ENTRY) V)))
         (CAR ENTRY))
        (SETQ ENTRY (CDR ENTRY))
        (GO LAB))
      ((LAMBDA (*EXPANDDF)
         (SETQ V
                 (EVALLETSUB (LIST (LIST RULELIST) (LIST 'SIMP* (MKQUOTE V)))
                             NIL)))
       T)
      (PROG (NEW)
        (SETQ NEW U)
       LAB
        (COND ((NULL NEW) (RETURN NIL)))
        ((LAMBDA (NEW)
           (PROG (OLD)
             (SETQ OLD F)
            LAB
             (COND ((NULL OLD) (RETURN NIL)))
             ((LAMBDA (OLD) (DEPEND1 NEW (CAR OLD) NIL)) (CAR OLD))
             (SETQ OLD (CDR OLD))
             (GO LAB)))
         (CAR NEW))
        (SETQ NEW (CDR NEW))
        (GO LAB))
      (RETURN V))) 
(PUT 'CHANGEVAR 'SIMPFN 'SIMPCHANGEVAR) 
(PUT 'CHANGEARG 'NUMBER-OF-ARGS 3) 
(PUT 'CHANGEARG 'DEFINED-ON-LINE '141) 
(PUT 'CHANGEARG 'DEFINED-IN-FILE 'MISC/CHANGEVR.RED) 
(PUT 'CHANGEARG 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE CHANGEARG (F U X)
    (COND ((ATOM X) X) ((MEMQ (CAR X) F) (CONS (CAR X) U))
          (T (CONS (CHANGEARG F U (CAR X)) (CHANGEARG F U (CDR X)))))) 
(PUT 'SUBCARE 'NUMBER-OF-ARGS 3) 
(PUT 'SUBCARE 'DEFINED-ON-LINE '147) 
(PUT 'SUBCARE 'DEFINED-IN-FILE 'MISC/CHANGEVR.RED) 
(PUT 'SUBCARE 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE SUBCARE (X Y Z)
    (COND ((NULL Z) NIL) ((EQUAL X Z) Y)
          ((OR (ATOM Z) (GET (CAR Z) 'SUBFUNC)) Z)
          (T (CONS (SUBCARE X Y (CAR Z)) (SUBCARE X Y (CDR Z)))))) 
(ENDMODULE) 