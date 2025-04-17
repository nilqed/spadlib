(cl:declaim (cl:optimize cl:debug cl:safety))
(cl:declaim (sb-ext:muffle-conditions sb-ext:compiler-note cl:style-warning))
(MODULE (LIST 'KREDELW)) 
(PUT 'GDIMENSION_EVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GDIMENSION_EVAL 'DEFINED-ON-LINE '30) 
(PUT 'GDIMENSION_EVAL 'DEFINED-IN-FILE 'GROEBNER/KREDELW.RED) 
(PUT 'GDIMENSION_EVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GDIMENSION_EVAL (U)
    (PROG (N M)
      (SETQ N 0)
      (SETQ M 0)
      (PROG (S)
        (SETQ S (CDR (GINDEPENDENT_SETEVAL U)))
       LAB
        (COND ((NULL S) (RETURN NIL)))
        ((LAMBDA (S)
           (COND ((GREATERP (SETQ M (LENGTH (CDR S))) N) (SETQ N M))))
         (CAR S))
        (SETQ S (CDR S))
        (GO LAB))
      (RETURN N))) 
(PUT 'GDIMENSION 'PSOPFN 'GDIMENSION_EVAL) 
(PUT 'GINDEPENDENT_SETEVAL 'NUMBER-OF-ARGS 1) 
(PUT 'GINDEPENDENT_SETEVAL 'DEFINED-ON-LINE '38) 
(PUT 'GINDEPENDENT_SETEVAL 'DEFINED-IN-FILE 'GROEBNER/KREDELW.RED) 
(PUT 'GINDEPENDENT_SETEVAL 'PROCEDURE_TYPE '(ARROW GENERAL GENERAL)) 
(DE GINDEPENDENT_SETEVAL (PARS)
    (PROG (A U V VARS W OLDORDER *FACTOR *EXP *GSUGAR *GROEBOPT)
      (SETQ *EXP T)
      (SETQ U (REVAL1 (CAR PARS) T))
      (SETQ V (COND ((CDR PARS) (REVAL1 (CADR PARS) T)) (T NIL)))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J (GROEREVLIST U))
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (J)
                                    (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                                  (CAR J))
                                 NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS
                         ((LAMBDA (J) (COND ((EQEXPR J) (*EQN2A J)) (T J)))
                          (CAR J))
                         NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (COND ((NULL W) (RERROR 'GROEBNR2 3 "empty list")))
      (SETQ A
              (COND
               ((AND GLOBAL-DIPVARS* (CDR GLOBAL-DIPVARS*))
                (CDR GLOBAL-DIPVARS*))
               (T (GVARLIS W))))
      (SETQ VARS
              (COND
               ((NULL V)
                (PROG (J FORALL-RESULT FORALL-ENDPTR)
                  (SETQ J A)
                  (COND ((NULL J) (RETURN NIL)))
                  (SETQ FORALL-RESULT
                          (SETQ FORALL-ENDPTR
                                  (CONS ((LAMBDA (J) (*A2K J)) (CAR J)) NIL)))
                 LOOPLABEL
                  (SETQ J (CDR J))
                  (COND ((NULL J) (RETURN FORALL-RESULT)))
                  (RPLACD FORALL-ENDPTR
                          (CONS ((LAMBDA (J) (*A2K J)) (CAR J)) NIL))
                  (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                  (GO LOOPLABEL)))
               (T (GROEREVLIST V))))
      (COND ((NOT VARS) (RETURN '(LIST))))
      (SETQ OLDORDER (VDPINIT VARS))
      (SETQ W
              (PROG (J FORALL-RESULT FORALL-ENDPTR)
                (SETQ J W)
                (COND ((NULL J) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (J) (CADR (A2VDP J))) (CAR J))
                                      NIL)))
               LOOPLABEL
                (SETQ J (CDR J))
                (COND ((NULL J) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (J) (CADR (A2VDP J))) (CAR J)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ VARS
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y VARS)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS
                                 ((LAMBDA (Y) (CONS Y (CADR (A2VDP Y))))
                                  (CAR Y))
                                 NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (CONS Y (CADR (A2VDP Y)))) (CAR Y))
                              NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (SETQ W (GROEBKWPREC VARS NIL W NIL))
      (RETURN
       (CONS 'LIST
             (PROG (S FORALL-RESULT FORALL-ENDPTR)
               (SETQ S W)
               (COND ((NULL S) (RETURN NIL)))
               (SETQ FORALL-RESULT
                       (SETQ FORALL-ENDPTR
                               (CONS
                                ((LAMBDA (S)
                                   (CONS 'LIST
                                         (REVERSIP
                                          (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                            (SETQ X S)
                                            (COND ((NULL X) (RETURN NIL)))
                                            (SETQ FORALL-RESULT
                                                    (SETQ FORALL-ENDPTR
                                                            (CONS
                                                             ((LAMBDA (X)
                                                                (CAR X))
                                                              (CAR X))
                                                             NIL)))
                                           LOOPLABEL
                                            (SETQ X (CDR X))
                                            (COND
                                             ((NULL X) (RETURN FORALL-RESULT)))
                                            (RPLACD FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (X) (CAR X))
                                                      (CAR X))
                                                     NIL))
                                            (SETQ FORALL-ENDPTR
                                                    (CDR FORALL-ENDPTR))
                                            (GO LOOPLABEL)))))
                                 (CAR S))
                                NIL)))
              LOOPLABEL
               (SETQ S (CDR S))
               (COND ((NULL S) (RETURN FORALL-RESULT)))
               (RPLACD FORALL-ENDPTR
                       (CONS
                        ((LAMBDA (S)
                           (CONS 'LIST
                                 (REVERSIP
                                  (PROG (X FORALL-RESULT FORALL-ENDPTR)
                                    (SETQ X S)
                                    (COND ((NULL X) (RETURN NIL)))
                                    (SETQ FORALL-RESULT
                                            (SETQ FORALL-ENDPTR
                                                    (CONS
                                                     ((LAMBDA (X) (CAR X))
                                                      (CAR X))
                                                     NIL)))
                                   LOOPLABEL
                                    (SETQ X (CDR X))
                                    (COND ((NULL X) (RETURN FORALL-RESULT)))
                                    (RPLACD FORALL-ENDPTR
                                            (CONS
                                             ((LAMBDA (X) (CAR X)) (CAR X))
                                             NIL))
                                    (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                                    (GO LOOPLABEL)))))
                         (CAR S))
                        NIL))
               (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
               (GO LOOPLABEL)))))) 
(PUT 'GINDEPENDENT_SETS 'PSOPFN 'GINDEPENDENT_SETEVAL) 
(PUT 'GROEBKWPREC 'NUMBER-OF-ARGS 4) 
(PUT 'GROEBKWPREC 'DEFINED-ON-LINE '61) 
(PUT 'GROEBKWPREC 'DEFINED-IN-FILE 'GROEBNER/KREDELW.RED) 
(PUT 'GROEBKWPREC 'PROCEDURE_TYPE
     '(ARROW (TIMES GENERAL GENERAL GENERAL GENERAL) GENERAL)) 
(DE GROEBKWPREC (VARS S LT M)
    (PROG (X S1 BOOL)
      (SETQ S1
              (PROG (Y FORALL-RESULT FORALL-ENDPTR)
                (SETQ Y S)
                (COND ((NULL Y) (RETURN NIL)))
                (SETQ FORALL-RESULT
                        (SETQ FORALL-ENDPTR
                                (CONS ((LAMBDA (Y) (CDR Y)) (CAR Y)) NIL)))
               LOOPLABEL
                (SETQ Y (CDR Y))
                (COND ((NULL Y) (RETURN FORALL-RESULT)))
                (RPLACD FORALL-ENDPTR
                        (CONS ((LAMBDA (Y) (CDR Y)) (CAR Y)) NIL))
                (SETQ FORALL-ENDPTR (CDR FORALL-ENDPTR))
                (GO LOOPLABEL)))
      (PROG ()
       WHILELABEL
        (COND ((NOT VARS) (RETURN NIL)))
        (PROGN
         (SETQ X (CAR VARS))
         (SETQ VARS (CDR VARS))
         (COND
          ((GROEBKWPREC1 (CONS (CDR X) S1) LT)
           (SETQ M (GROEBKWPREC VARS (CONS X S) LT M)))))
        (GO WHILELABEL))
      (SETQ BOOL T)
      (PROG (Y)
        (SETQ Y M)
       LAB
        (COND ((NULL Y) (RETURN NIL)))
        ((LAMBDA (Y)
           (SETQ BOOL
                   (AND BOOL
                        (NOT (EQUAL (LENGTH S) (LENGTH (INTERSECTION S Y)))))))
         (CAR Y))
        (SETQ Y (CDR Y))
        (GO LAB))
      (RETURN (COND (BOOL (CONS S M)) (T M))))) 
(PUT 'GROEBKWPREC1 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBKWPREC1 'DEFINED-ON-LINE '78) 
(PUT 'GROEBKWPREC1 'DEFINED-IN-FILE 'GROEBNER/KREDELW.RED) 
(PUT 'GROEBKWPREC1 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBKWPREC1 (S LT)
    (COND ((NULL LT) T)
          (T (AND (GROEBKWPREC2 S (CAR LT)) (GROEBKWPREC1 S (CDR LT)))))) 
(PUT 'GROEBKWPREC2 'NUMBER-OF-ARGS 2) 
(PUT 'GROEBKWPREC2 'DEFINED-ON-LINE '82) 
(PUT 'GROEBKWPREC2 'DEFINED-IN-FILE 'GROEBNER/KREDELW.RED) 
(PUT 'GROEBKWPREC2 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE GROEBKWPREC2 (S MON)
    (PROGN
     (PROG (M)
       (SETQ M S)
      LAB
       (COND ((NULL M) (RETURN NIL)))
       ((LAMBDA (M) (SETQ MON (VEVCAN0 M MON))) (CAR M))
       (SETQ M (CDR M))
       (GO LAB))
     (NOT (OR (NULL MON) (AND (EQUAL (CAR MON) 0) (VEVZERO?1 (CDR MON))))))) 
(PUT 'VEVCAN0 'NUMBER-OF-ARGS 2) 
(PUT 'VEVCAN0 'DEFINED-ON-LINE '86) 
(PUT 'VEVCAN0 'DEFINED-IN-FILE 'GROEBNER/KREDELW.RED) 
(PUT 'VEVCAN0 'PROCEDURE_TYPE '(ARROW (TIMES GENERAL GENERAL) GENERAL)) 
(DE VEVCAN0 (M MON)
    (COND ((OR (NULL M) (AND (EQUAL (CAR M) 0) (VEVZERO?1 (CDR M)))) MON)
          ((OR (NULL MON) (AND (EQUAL (CAR MON) 0) (VEVZERO?1 (CDR MON)))) NIL)
          (T
           (CONS (COND ((NEQ (CAR M) 0) 0) (T (CAR MON)))
                 (VEVCAN0 (CDR M) (CDR MON)))))) 
(ENDMODULE) 