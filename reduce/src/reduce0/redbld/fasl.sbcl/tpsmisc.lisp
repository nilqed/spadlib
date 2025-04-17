(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'TPSMISC)) 
(FLUID '(GENSYM-LIST)) 
(PUT '|PS:COPY| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:COPY| 'DEFINED-ON-LINE '34) 
(PUT '|PS:COPY| 'DEFINED-IN-FILE 'TPS/TPSMISC.RED) 
(PUT '|PS:COPY| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PS:COPY| (PS)
    (PROG (GENSYM-LIST NEW)
      (SETQ NEW (|PS:COPY1| PS))
      (COND (GENSYM-LIST (FIX-UP-LINKS PS NEW)))
      (RETURN NEW))) 
(PUT '|PS:COPY1| 'NUMBER-OF-ARGS 1) 
(PUT '|PS:COPY1| 'DEFINED-ON-LINE '41) 
(PUT '|PS:COPY1| 'DEFINED-IN-FILE 'TPS/TPSMISC.RED) 
(PUT '|PS:COPY1| 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE |PS:COPY1| (PS)
    (COND
     ((OR
       ((LAMBDA (U)
          (OR (ATOM U) (AND (NEQ (CAR U) '|:PS:|) (GET (CAR U) 'DNAME))))
        PS)
       (NEQ (CAR PS) '|:PS:|))
      PS)
     (T
      (PROG (NEW OLD NEWEXP)
        (SETQ OLD (CDR PS))
        (COND
         ((IDP OLD)
          (PROGN
           (SETQ NEW (GENSYM))
           (SETQ GENSYM-LIST (CONS (CONS (EVAL OLD) NEW) GENSYM-LIST))
           (RETURN (CONS (CAR PS) NEW)))))
        (SETQ NEW (MKVECT 7))
        (PROG (I)
          (SETQ I 0)
         LAB
          (COND ((MINUSP (DIFFERENCE 7 I)) (RETURN NIL)))
          (PUTV NEW I (GETV OLD I))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (SETQ OLD (|PS:EXPRESSION| PS))
        (SETQ NEW (CONS (CAR PS) NEW))
        (COND
         ((LISTP OLD)
          (PROGN
           (SETQ NEWEXP
                   (CONS (CAR OLD)
                         (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                           (SETQ ARG (CDR OLD))
                           (COND ((NULL ARG) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (ARG) (|PS:COPY1| ARG))
                                             (CAR ARG))
                                            NIL)))
                          LOOPLABEL
                           (SETQ ARG (CDR ARG))
                           (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ARG) (|PS:COPY1| ARG)) (CAR ARG))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
           (|PS:PUTV| NEW 6 NEWEXP)
           (COND
            ((EQUAL (CAR OLD) 'PSGEN)
             (PROGN
              (SETQ NEWEXP
                      (|PS:REPLACE| (CADR OLD) (CADR NEWEXP)
                       ((LAMBDA (PS)
                          (COND
                           ((OR (ATOM PS)
                                (AND (NEQ (CAR PS) '|:PS:|)
                                     (GET (CAR PS) 'DNAME)))
                            (LIST (CONS 0 (CONS PS 1))))
                           (T (|PS:GETV| PS 5))))
                        PS)))
              (|PS:PUTV| NEW 5 NEWEXP)))))))
        (RETURN NEW))))) 
(PUT '|PS:REPLACE| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:REPLACE| 'DEFINED-ON-LINE '66) 
(PUT '|PS:REPLACE| 'DEFINED-IN-FILE 'TPS/TPSMISC.RED) 
(PUT '|PS:REPLACE| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:REPLACE| (P Q TERMS)
    (PROG (TERM FORALL-RESULT FORALL-ENDPTR)
      (SETQ TERM TERMS)
      (COND ((NULL TERM) (RETURN NIL)))
      (SETQ FORALL-RESULT
              (SETQ FORALL-ENDPTR
                      (CONS
                       ((LAMBDA (TERM)
                          (CONS (CAR TERM) (|PS:COPY2| (CDR TERM) P Q)))
                        (CAR TERM))
                       NIL)))
     LOOPLABEL
      (SETQ TERM (CDR TERM))
      (COND ((NULL TERM) (RETURN FORALL-RESULT)))
      (RPLACD FORALL-ENDPTR
              (CONS
               ((LAMBDA (TERM) (CONS (CAR TERM) (|PS:COPY2| (CDR TERM) P Q)))
                (CAR TERM))
               NIL))
      (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
      (GO LOOPLABEL))) 
(PUT '|PS:COPY2| 'NUMBER-OF-ARGS 3) 
(PUT '|PS:COPY2| 'DEFINED-ON-LINE '69) 
(PUT '|PS:COPY2| 'DEFINED-IN-FILE 'TPS/TPSMISC.RED) 
(PUT '|PS:COPY2| 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL) GENERAL)) 
(DE |PS:COPY2| (PS P Q)
    (COND
     ((OR
       ((LAMBDA (U)
          (OR (ATOM U) (AND (NEQ (CAR U) '|:PS:|) (GET (CAR U) 'DNAME))))
        PS)
       (NEQ (CAR PS) '|:PS:|))
      PS)
     ((EQUAL PS P) Q)
     (T
      (PROG (NEW OLD NEWEXP)
        (SETQ OLD (CDR PS))
        (SETQ NEW (MKVECT 7))
        (PROG (I)
          (SETQ I 0)
         LAB
          (COND ((MINUSP (DIFFERENCE 7 I)) (RETURN NIL)))
          (PUTV NEW I (GETV OLD I))
          (SETQ I (PLUS2 I 1))
          (GO LAB))
        (SETQ OLD (|PS:EXPRESSION| PS))
        (SETQ NEW (CONS (CAR PS) NEW))
        (COND
         ((LISTP OLD)
          (PROGN
           (SETQ NEWEXP
                   (CONS (CAR OLD)
                         (PROG (ARG FORALL-RESULT FORALL-ENDPTR)
                           (SETQ ARG (CDR OLD))
                           (COND ((NULL ARG) (RETURN NIL)))
                           (SETQ FORALL-RESULT
                                   (SETQ FORALL-ENDPTR
                                           (CONS
                                            ((LAMBDA (ARG)
                                               (|PS:COPY2| ARG P Q))
                                             (CAR ARG))
                                            NIL)))
                          LOOPLABEL
                           (SETQ ARG (CDR ARG))
                           (COND ((NULL ARG) (RETURN FORALL-RESULT)))
                           (RPLACD FORALL-ENDPTR
                                   (CONS
                                    ((LAMBDA (ARG) (|PS:COPY2| ARG P Q))
                                     (CAR ARG))
                                    NIL))
                           (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                           (GO LOOPLABEL))))
           (|PS:PUTV| NEW 6 NEWEXP)
           NIL)))
        (RETURN NEW))))) 
(PUT 'FIX-UP-LINKS 'NUMBER-OF-ARGS 2) 
(PUT 'FIX-UP-LINKS 'DEFINED-ON-LINE '92) 
(PUT 'FIX-UP-LINKS 'DEFINED-IN-FILE 'TPS/TPSMISC.RED) 
(PUT 'FIX-UP-LINKS 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE FIX-UP-LINKS (P Q)
    (COND
     ((OR (OR (ATOM P) (AND (NEQ (CAR P) '|:PS:|) (GET (CAR P) 'DNAME)))
          (NEQ (CAR P) '|:PS:|))
      NIL)
     (T
      (PROG (X ARGS1 ARGS2)
        (COND ((SETQ X (ASSOC (CDR P) GENSYM-LIST)) (SET (CDR X) (CDR Q))))
        (COND
         ((NOT (IDP (CDR P)))
          (PROGN
           (SETQ X (|PS:EXPRESSION| P))
           (COND
            ((LISTP X)
             (PROGN
              (SETQ ARGS1 (CDR X))
              (SETQ ARGS2 (CDR (|PS:EXPRESSION| Q)))
              (PROG ()
               WHILELABEL
                (COND ((NOT ARGS1) (RETURN NIL)))
                (PROGN
                 (FIX-UP-LINKS (CAR ARGS1) (CAR ARGS2))
                 (SETQ ARGS1 (CDR ARGS1))
                 (SETQ ARGS2 (CDR ARGS2)))
                (GO WHILELABEL)))))))))))) 
(PUT 'PSCOPY 'SIMPFN 'SIMPPSCOPY) 
(PUT 'SIMPPSCOPY 'NUMBER-OF-ARGS 1) 
(PUT 'SIMPPSCOPY 'DEFINED-ON-LINE '112) 
(PUT 'SIMPPSCOPY 'DEFINED-IN-FILE 'TPS/TPSMISC.RED) 
(PUT 'SIMPPSCOPY 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE SIMPPSCOPY (U)
    (PROGN
     (SETQ U (PREPSQXX (SIMP* (CARX U 'PSCOPY))))
     (COND ((AND (PAIRP U) (EQUAL (CAR U) '|:PS:|)) (SIMP* (|PS:COPY| U)))
           (T (TYPERR U "power series:  simppscopy"))))) 
(ENDMODULE) 